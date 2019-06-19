###
### mendoza/template.janet
### Copyright © Calvin Rose 2019
###

(import mendoza/watch-cache :as wc)
(def- base-env (require "mendoza/template-env"))
(table/setproto base-env (table/getproto (fiber/getenv (fiber/current))))

(defn- template
  "Compile a bar template string into a function."
  [source &opt where]

  (default where source)
  (def env (table/setproto @{} base-env))
  (def bufsym (gensym))

  # State for compilation machine
  (def p (parser/new))
  (def forms @[])

  (defn compile-time-chunk
    "Eval the capture straight away during compilation. Use for imports, etc."
    [chunk]
    (defn do-in-env [] (eval-string chunk))
    (def f (fiber/new do-in-env))
    (fiber/setenv f env)
    (resume f)
    true)

  (defn parse-chunk
    "Parse a string and push produced values to forms."
    [chunk]
    (parser/consume p chunk)
    (while (parser/has-more p)
      (array/push forms (parser/produce p))))

  (defn code-chunk
    "Parse all the forms in str and insert them into the template."
    [str]
    (parse-chunk str)
    (if (= :error (parser/status p))
      (error (parser/error p)))
    true)

  (defn sub-chunk
    "Same as code-chunk, but results in sending code to the buffer."
    [str]
    (code-chunk
      (string " (render " str " " bufsym " state) ")))

  (defn string-chunk
    "Insert string chunk into parser"
    [str]
    (parser/insert p ~(,buffer/push-string ,bufsym ,str))
    (parse-chunk "")
    true)

  # Run peg
  (def grammar
    ~{:code-chunk (* "{%" (drop (cmt '(any (if-not "%}" 1)) ,code-chunk)) "%}")
      :compile-time-chunk (* "{$" (drop (cmt '(any (if-not "$}" 1)) ,compile-time-chunk)) "$}")
      :sub-chunk (* "{{" (drop (cmt '(any (if-not "}}" 1)) ,sub-chunk)) "}}")
      :main-chunk (drop (cmt '(any (if-not (+ "{$" "{{" "{%") 1)) ,string-chunk))
      :main (any (+ :compile-time-chunk :code-chunk :sub-chunk :main-chunk (error "")))})
  (def did-match (peg/match grammar source))

  # Check errors in template and parser
  (unless did-match (error "invalid template syntax"))
  (parse-chunk "\n")
  (parser/eof p)
  (case (parser/status p)
    :error (error (parser/error p)))

  # Make ast from forms
  (def ast ~(fn _mendoza-template [,bufsym state render]
              # Add important bindings to make templating easier.
              # Also helps catching template errors.
              (def pages (state :pages))
              (def url (state :url))
              (def content (state :content))
              (def sitemap (state :sitemap))
              ,;forms
              ,bufsym))

  (def ctor (compile ast env (string where ":gen")))
  (if-not (function? ctor)
    (error (string "could not compile template: " (string/format "%p" ctor))))

  (let [f (fiber/new ctor :e)]
    (fiber/setenv f env)
    (def res (resume f))
    (case res
      :error (error res)
      res)))

#
# Module loading
#

(defn add-loader
  "Adds the custom template loader to Janet's module/loaders."
  []
  (put module/loaders :mendoza-template (fn [x &] 
                                          (wc/add (template (slurp x) x))))
  (array/insert module/paths 0 ["./templates/:all:" :mendoza-template ".html"])
  (array/insert module/paths 1 ["./mendoza/templates/:all:" :mendoza-template ".html"])
  (array/insert module/paths 2 [":sys:/mendoza/templates/:all:" :mendoza-template ".html"]))
