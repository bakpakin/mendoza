###
### mendoza/markup.janet
### Copyright © Calvin Rose 2019
###

(import mendoza/watch-cache :as wc)
(def- base-env (require "mendoza/markup-env"))
(table/setproto base-env (table/getproto (fiber/getenv (fiber/current))))

(defn- capture-front
  "Capture the front matter"
  [chunk]
  (def p (parser/new))
  (parser/consume p chunk)
  (parser/eof p)
  (def ret @[])
  (while (parser/has-more p) (array/push ret (parser/produce p)))
  ret)

(defn- capture-value
  "Parse a janet value capture in a pattern. At this point, we
  should already know that the source is valid."
  [chunk]
  (def p (parser/new))
  (parser/consume p chunk)
  (parser/eof p)
  (parser/produce p))

(defn- capture-node
  "Capture a node in the grammar."
  [name & params]
  ~(,(symbol name) ,;params))

(def- symchars
  "peg for valid symbol characters."
  '(+ (range "09" "AZ" "az" "\x80\xFF") (set "!$%&*+-./:<?=>@^_|")))

(def- value-grammar
  "Grammar to get the source for a valid janet value. As it
  doesn't parse the source, it can be a bit shorter and simpler."
  ~{:ws (set " \v\t\r\f\n\0")
    :readermac (set "';~,")
    :symchars ,symchars
    :token (some :symchars)
    :hex (range "09" "af" "AF")
    :escape (* "\\" (+ (set "ntrvzf0e\"\\") (* "x" :hex :hex)))
    :comment (* "#" (any (if-not (+ "\n" -1) 1)))
    :symbol (if-not (range "09") :token)
    :bytes (* (? "@") "\"" (any (+ :escape (if-not "\"" 1))) "\"")
    :long-bytes {:delim (some "`")
                 :open (capture :delim :n)
                 :close (cmt (* (not (> -1 "`")) (-> :n) ':delim) ,=)
                 :main (drop (* (? "@") :open (any (if-not :close 1)) :close))}
    :number (drop (cmt ':token ,scan-number))
    :raw-value (+ :comment :number :bytes :long-bytes
                  :ptuple :btuple :struct :symbol)
    :value (* (any (+ :ws :readermac)) :raw-value)
    :root (any :value)
    :root2 (any (* :value :value))
    :ptuple (* (? "@") "(" :root (any :ws) ")")
    :btuple (* (? "@") "[" :root (any :ws) "]")
    :struct (* (? "@") "{" :root2 (any :ws) "}")
    :main (/ ':value ,capture-value)})

# Some capture functions to make markup a bit
# more like markdown. This is useful in the common
# case.
(defn- capp [& content] (unless (empty? content)
                          {:tag "p" :content (array/slice content)}))
(defn- caph [n & content] {:tag (string "h" (length n)) :content
                           (array/slice content)})

(def- markup-grammar
  "Grammar for markdown -> document AST parser."
  ~{
    # basic character classes
    :wsnl (set " \t\r\v\f\n")
    :ws (set " \t\r\v\f")

    # A span of markup that is not line delimited (most markup)
    :char (+ (* "\\" 1) (if-not (set "@}") 1))
    :leaf (/ '(some :char) ,(partial string/replace "\\" ""))
    :root (some (+ :node :leaf))

    # A span or markup that is line delimited (headers, etc). @ expressions
    # can still cross line boundaries.
    :char-line (+ (* "\\" 1) (if-not (set "@}\n") 1))
    :leaf-line (/ '(* (some :char-line) (? "\n")) ,(partial string/replace "\\" ""))
    :root-line (some (+ (* :node (? '"\n")) :leaf-line))

    # An @ expression (a node)
    :node {:paren-params (* "(" (any :wsnl) (any (* :janet-value (any :wsnl))) ")")
           :string-param (* (> 0 "\"") :janet-value)
           :longstring-param (* (> 0 "`") :janet-value)
           :curly-params (* "{" (/ (any :root) ,array) "}")
           :bracket-params (* "[" '(any (if-not "]" 1)) "]")
           :params (any (* (any :wsnl) (+ :bracket-params :curly-params :paren-params :string-param :longstring-param)))
           :name '(if-not (range "09") (some ,symchars))
           :main (/ (* "@" :name :params) ,capture-node)}

    # Pretty errors
    :err (error (/ (* '1 (position))
                   ,(fn [x p] (string "unmatched character "
                                      (describe x)
                                      " at byte " p))))

    # Front matter
    :front (/ '(any (if-not "---" 1)) ,capture-front)

    :janet-value ,value-grammar

    # Headers (only at top level)
    :header (/ (* '(between 1 6 "#") (any :ws) :root-line) ,caph)

    # Main rule: Front matter -> Top level nodes and markup
    :main (* :front "---" (any (+ '(some :wsnl)
                                  (* :node (any :wsnl))
                                  :header
                                  (/ :root-line ,capp)
                                  "}"))
             (+ -1 :err))})

(def- markup-peg
  "A peg that converts markdown to html."
  (peg/compile markup-grammar))

(defn markup
  "Parse mendoza markup and evaluate it returning a document tree."
  [source]
  (def env (table/setproto @{} base-env))
  (def matches (peg/match markup-peg source))
  (unless matches (error "bad markdown"))
  (def front-matter (matches 0))
  (defn do-contents []
      (loop [ast :in (tuple/slice front-matter 0 -2)]
        (eval ast))
      (def matter (eval (last front-matter)))
      (merge matter
             {:content (seq [ast :in (tuple/slice matches 1)]
                            (eval ast))}))
  (def f (fiber/new do-contents :e))
  (fiber/setenv f env)
  (def res (resume f))
  (case (fiber/status f)
    :error (error res)
    res))

#
# Module loading
#

(defn add-loader
  "Adds the custom markup loader to Janet's module/loaders."
  []
  (put module/loaders :mendoza-markup (fn [x &] 
                                        (wc/add (markup (slurp x)))))
  (array/insert module/paths 0 [":all:" :mendoza-markup ".mdz"])
  (array/insert module/paths 1 ["./content/:all:" :mendoza-markup ".mdz"]))
