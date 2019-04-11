###
### mendoza.janet
### Copyright © 2019 Calvin Rose
###

(def version "0.0.0")

#
# Template Syntax and Compiler
#

(def- _env *env*)
(defn- make-template-env
  "Creates an environment that has access to both
  symbols in the current environment and mendoza's
  symbols."
  []
  (let [e (make-env *env*)]
    (loop [[sym entry] :pairs _env
           :when (not (entry :private))]
      (put e sym entry))
    e))

(defn template
  "Compile a bar template string into a function."
  [source &opt where]

  (default where source)
  (def env (make-template-env))

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
  (def ast ~(fn _template [,bufsym state]
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
(defn require-template
  "Require a template. A template can either be an HTML template, or
  a janet source file that is loaded in the normal manner."
  [name]
  (def name (if (string/find ".html" name)
              name
              (string name ".html")))
  (if-let [ret (loaded-templates name)]
    ret
    (let [path (string "templates/" name)
          _ (print "Requiring " path " as bar template...")
          source (slurp path)
          t (template source path)]
      (put loaded-templates name t)
      t)))

#
# Built in Markup Macros
#

(each tag ["h1" "h2" "h3" "h4" "h5" "h6" "ul" "ol" "li" "p" "em" "strong" "u" "pre"]
  (defglobal tag (fn [& content] {:tag tag :content content})))
(defn bigger [& content] {:tag "span" "style" "font-size:1.61803398875em;" :content content})
(defn smaller [& content] {:tag "span" "style" "font-size:0.61803398875em;" :content content})
(defn code
  "Inline code or codeblock"
  [lang &opt source]
  (def source2 (or source lang))
  (def lang2 (if source lang nil))
  {:tag "pre" :content {:tag "code"
                        :content source2
                        :language lang2
                        "data-language" lang2}})

#
# Mendoza Markup -> DOM parser
#

(defn- capture-value
  "Parse a janet value capture in a pattern. At this point, we
  should already know that the source is valid."
  [chunk]
  (def p (parser/new))
  (parser/consume p chunk)
  (parser/eof p)
  (parser/produce p))

(defn- quoter [x] ~',x)
(defn- capture-node
  "Capture a node in the grammar."
  [name params body env]
  (def source ~(,(symbol name) ,;params ,;(map quoter body)))
  (eval source env))

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

(defn- capp [& content] (unless (empty? content) {:tag "p" :content content}))

(def- markup-grammar
  "Grammar for markdown -> document AST parser."
  ~{:wsnl (set " \t\r\v\f\n")
    :ws (set " \t\r\v\f")
    :linechar (+ (* "\\" 1) (if-not (set "@}\n") 1))
    :leaf (/ '(* (some :linechar) (? "\n")) ,(partial string/replace "\\" ""))
    :janet-value (+ ,value-grammar (error (position)))
    :root (some (+ :leaf :node))
    :node {:params (+ (* (> 0 "[") :janet-value) (constant []))
           :body (+ (* "{" (group (any :root)) "}") (constant []))
           :name '(if-not (range "09") (some ,symchars))
           :main (/ (* "@" :name :params :body (argument 0)) ,capture-node)}
    :block (/ (* (some (+ :node :leaf)) (+ "\n" "}" -1)) ,capp)
    :err (error (/ (* '1 (position))
                   ,(fn [x p] (string "unmatched character "
                                      (describe x)
                                      " at byte " p))))
    :front (/ (* '(any (if-not "---" 1)) (argument 0)) ,eval-string)
    :main (* :front "---" (any (+ '(some :wsnl) :node :block "}" "\n")) (+ -1 :err))})

(def- markup-peg
  "A peg that converts markdown to html."
  (peg/compile markup-grammar))

(defn markup
  "Parse mendoza markup and evaluate it, returning a dom."
  [source]
  (def env (make-template-env))
  (def matches (peg/match markup-peg source 0 env))
  (unless matches (error "bad markdown"))
  (def ret ~@{:content ,(tuple/slice matches 1)})
  (loop [[k v] :pairs (matches 0) :when (keyword? k)]
    (put ret k v))
  ret)

#
# DOM -> HTML rendering
#

(def- html-escape-chars
  "Characters to escape for HTML"
  {("&" 0) "&amp;"
   ("<" 0) "&lt;"
   (">" 0) "&gt;"})

(def- attribute-escape-chars
  "Characters to escape for HTML"
  {("\"" 0) "&quot;"
   ("'" 0) "&#39;"})

(defn- escape
  "Escape a string into buf."
  [str buf escapes]
  (each byte str
    (if-let [e (escapes byte)]
      (buffer/push-string buf e)
      (buffer/push-byte buf byte))))

(def- language-highlighters
  "Syntax highlighters for all languages. Grammars
   are loaded lazily."
  @{})

(defn- get-highlighter-grammar
  "Load a highlighter if not already loaded. Otherwise, get cached peg."
  [name]
  (if-let [peg (language-highlighters name)]
    peg
    (do
      (require (string "syntax/" name))
      (language-highlighters name))))

(def- html-colors
  "Default theme colors for syntax highlighting."
  {:number "#89dc76"
   :keyword "#ffd866"
   :string "#ab90f2"
   :coresym "#ff6188"
   :constant "#fc9867"
   :character "red"
   :identifier "white"
   :comment "gray"
   :operator "white"
   :type "green"
   :line "gray"})

(defn- highlight-genhtml
  "Paint colors for HTML"
  [buf tokens colors]
  (each token tokens
    (if (bytes? token)
      (escape token buf html-escape-chars)
      (let [[class bytes] token
            color (colors class)]
        (if color
          (do
            (buffer/push-string buf "<span style=\"color:" color "\">")
            (escape bytes buf html-escape-chars)
            (buffer/push-string buf "</span>"))
          (escape bytes buf html-escape-chars))))))

(defn span
  "Create a replacer function for a peg grammar that is used to capture
  and color output."
  [class]
  (if (not (html-colors class))
    (error (string "invalid class " class))
    (fn [text]  [class text])))

(defn add-syntax
  "Define a grammar for syntax highlighting. This just registers a
  grammar that will be used for a given language."
  [name grammar]
  (def peg (if (= :core/peg (type grammar)) grammar (peg/compile grammar)))
  (put language-highlighters name peg)
  nil)

(defn render
  "Render a document node into HTML. Returns a buffer."
  [node buf state]
  (cond
    (buffer? node) (buffer/push-string buf node)
    (bytes? node) (escape node buf html-escape-chars)
    (indexed? node) (each c node (render c buf state))
    (dictionary? node)
    (let [tag (node :tag)]
      (when tag
        (buffer/push-string buf "<" tag)
        (loop [k :keys node :when (string? k)]
          (buffer/push-string buf " " k "=\"")
          (escape (node k) buf attribute-escape-chars)
          (buffer/push-string buf "\""))
        (buffer/push-string buf ">"))
      (if-let [lang (node :language)]
        (highlight-genhtml buf
                           (peg/match (get-highlighter-grammar lang) (node :content))
                           (or (state :colors) html-colors))
        (if-let [temp (node :template)]
          ((require-template temp) buf (merge state node))
          (render (node :content) buf state)))
      (when (and tag (not (node :no-close)))
        (buffer/push-string buf "</" tag ">")))
    (number? node) (buffer/push-string buf (string node)))
  buf)

#
# File System Helpers
#

(defn- cp-rf
  "Copy files recursively. Does not copy file permissions, but that's ok for
  a static site."
  [src dest]
  (os/mkdir dest)
  (each f (os/dir src)
    (let [subsrc (string src "/" f)
          subdest (string dest "/" f)]
      (if (= (os/stat subsrc :mode) :directory)
        (cp-rf subsrc subdest)
        (spit subdest (slurp subsrc))))))

(defn- create-dirs
  "Recursively create directories for a path if they don't exist"
  [url]
  (def parts (tuple/slice (string/split "/" url) 0 -2))
  (def buf @"")
  (each part parts
    (buffer/push-string buf part)
    (def path (string buf))
    (unless (= (os/stat path :mode) :directory)
      (os/mkdir path))
    (buffer/push-string buf "/")))

(defn- page-get-url
  "Get the output url for a dom"
  [page]
  (def o (page :url))
  (or o (string (string/slice (page :input) 7 -5) ".html")))

(defn- rimraf
  "Remove a directory and all sub directories."
  [path]
  (if-let [m (os/stat path :mode)]
    (if (= m :directory)
      (do
        (each subpath (os/dir path) (rimraf (string path "/" subpath)))
        (os/rmdir path))
      (os/rm path))))

#
# Main API
#

(defn clean
  "Clean up the old site."
  []
  (print "Removing directory site...")
  (rimraf "site"))

(defn serve
  "Serve the site locally."
  [&opt port]
  (default port "8000")
  (os/shell (string "cd site; python3 -m http.server " port)))

(defn build
  "Build the static site and put it in the output folder."
  []

  # Clean up old artifacts
  (clean)
  (os/mkdir "site")

  # Copy static stuff
  (when (os/stat "static" :mode)
    (cp-rf "static" "site"))

  # Read in pages
  (def pages @[])
  (defn read-pages [path]
    (case (os/stat path :mode)
      :directory (each f (sort (os/dir path))
                   (read-pages (string path "/" f)))
      :file (when (= ".mdz" (string/slice path -5))
              (print "Parsing content " path " as mendoza markup")
              (def page (markup (slurp path)))
              (put page :input path)
              (put page :url (page-get-url page))
              (array/push pages page))))
  (read-pages "content")

  # Render a page
  (defn render-page
    [page url]
    (def state @{:url url :pages pages})
    (def out (render page @"" state))
    (def outpath (string "site" url))
    (print "Writing HTML to " outpath "...")
    (create-dirs outpath)
    (spit outpath out))

  # Render all pages
  (loop [page :in pages]
    (def url (page :url))
    (render-page page url)))
