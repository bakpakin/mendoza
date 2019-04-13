###
### mendoza/template.janet
### Copyright © Calvin Rose 2019
###

(defn- template
  "Compile a bar template string into a function."
  [source &opt where]

  (default where source)
  (def env (table/setproto @{} *env*))
  (def bufsym (gensym))

  # State for compilation machine
  (def p (parser/new))
  (def forms @[])

  (defn compile-time-chunk
    "Eval the capture straight away during compilation. Use for imports, etc."
    [chunk]
    (eval-string chunk env)
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
  (def ast ~(fn _template [,bufsym state render]
              # Add important bindings to make templating easier.
              # Also helps catching template errors.
              (def pages (state :pages))
              (def url (state :url))
              (def content (state :content))
              ,;forms
              ,bufsym))

  (def ctor (compile ast env (string where ":gen")))
  (if-not (function? ctor)
    (error (string "could not compile template: " (string/format "%p" ctor))))
  (ctor))

(def- loaded-templates @{})

(defn- norm-name
  [name]
  (if (string/find ".html" name)
    name
    (string name ".html")))

(defn load
  "Require a template. A template can either be an HTML template, or
  a janet source file that is loaded in the normal manner."
  [name]
  (def name (norm-name name))
  (if-let [ret (loaded-templates name)]
    ret
    (let [path (string "templates/" name)
          _ (print "Requiring " path " as bar template...")
          source (slurp path)
          t (template source path)]
      (put loaded-templates name t)
      t)))

(defn unload
  "Unload all loaded templates"
  []
  (loop [name :keys loaded-templates]
    (put loaded-templates (norm-name name) nil)))
