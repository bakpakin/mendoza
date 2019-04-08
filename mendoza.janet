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
  (if-let [ret (loaded-templates name)]
    ret
    (if (= ".html" (string/slice name -6))
      # Bar html template
      (let [path (string "templates/" name)
            _ (print "Requiring " path " as bar template...")
            source (slurp path)
            t (template source path)]
        (put loaded-templates name t)
        t)
      # Janet template - useful for codeblocks and non html renderers
      (let [path (string "templates/" name)
            _ (print "Requiring " path " as janet template...")
            env (require path :env (table/setproto @{} _env))
            main-entry (env 'main)
            t (main-entry :value)]
        (put loaded-templates name t)
        t))))

#
# Markdown -> DOM parser
#

(defn- capture-header
  "Peg capture function for parsing markdown headers."
  [prefix & content]
  (def n (length prefix))
  {:tag (string "h" n)
   :content content})

(defn- capture-anchor
  "Peg capture function for parsing markdown links."
  [& args]
  {:tag "a"
   :content (tuple/slice args 0 -2)
   "href" (last args)})

(defn- capture-image
  "Peg capture function for parsing markdown images."
  [& args]
  {:tag "img"
   :content (tuple/slice args 0 -2)
   :no-close true
   "src" (last args)})

(defn- line-el
  "Create a peg pattern for parsing some markup
  on a line. Captures the HTML text to output."
  [delim tag]
  (defn replace
    [& args]
    {:tag tag
     :content args})
  ~(* ,delim
      (/ (any (if-not ,delim :token)) ,replace)
      ,delim))

(defn- capture-table
  "Peg capture function for a table"
  [& contents]
  {:tag "table"
   :content contents})

(defn- capture-code
  "Peg capture function for single line of code"
  [code]
  {:tag "code" :content [code]})

(defn- capture-codeblock
  "Peg capture function for multiline codeblock"
  [language code]
  {:tag "pre"
   :content {:tag "code"
             :content code
             :template (if (= "" language) nil language)}})

(defn- capture-li
  "Capture a list inside the peg and create a Document Node."
  [& contents]
  {:tag "li"
   :content contents})

(defn- capture-ol
  "Capture a list inside the peg and create a Document Node."
  [& contents]
  {:tag "ol"
   :content contents})

(defn- capture-ul
  "Capture a list inside the peg and create a Document Node."
  [& contents]
  {:tag "ul"
   :content contents})

(defn- capture-paragraph
  "Capture a paragraph node in the peg."
  [& contents]
  {:tag "p"
   :content contents})

(defn- capture-value
  "Parse a janet value capture in a pattern. At this point, we
  should already know that the source is valid."
  [chunk]
  (def p (parser/new))
  (parser/consume p chunk)
  (parser/eof p)
  (parser/produce p))

(def- md-grammar
  "Grammar for markdown -> document AST parser."
  ~{

    # Sub grammar for recognizing a janet value.
    :janet-value {:ws (set " \v\t\r\f\n\0")
                  :readermac (set "';~,")
                  :symchars (+ (range "09" "AZ" "az" "\x80\xFF") (set "!$%&*+-./:<?=>@^_|"))
                  :token (some :symchars)
                  :hex (range "09" "af" "AF")
                  :escape (* "\\" (+ (set "ntrvzf0e\"\\")
                                     (* "x" :hex :hex)
                                     (error (constant "bad hex escape"))))
                  :comment (* "#" (any (if-not (+ "\n" -1) 1)))
                  :symbol (if-not (range "09") :token)
                  :keyword (* ":" (any :symchars))
                  :constant (+ "true" "false" "nil")
                  :bytes (* "\"" (any (+ :escape (if-not "\"" 1))) "\"")
                  :string :bytes
                  :buffer (* "@" :bytes)
                  :long-bytes {:delim (some "`")
                               :open (capture :delim :n)
                               :close (cmt (* (not (> -1 "`")) (-> :n) ':delim) ,=)
                               :main (drop (* :open (any (if-not :close 1)) :close))}
                  :long-string :long-bytes
                  :long-buffer (* "@" :long-bytes)
                  :number (drop (cmt ':token ,scan-number))
                  :raw-value (+ :comment :constant :number :keyword
                                :string :buffer :long-string :long-buffer
                                :parray :barray :ptuple :btuple :struct :dict :symbol)
                  :value (* (any (+ :ws :readermac)) :raw-value)
                  :root (any :value)
                  :root2 (any (* :value :value))
                  :ptuple (* "(" :root (any :ws) (+ ")" (error "bad janet form")))
                  :btuple (* "[" :root (any :ws) (+ "]" (error "bad janet bracketed form")))
                  :struct (* "{" :root2 (any :ws) (+ "}" (error "bad janet dictionary")))
                  :parray (* "@" :ptuple)
                  :barray (* "@" :btuple)
                  :dict (* "@" :struct)
                  :main (/ '(+ :value (error "bad janet value")) ,capture-value)}

    :next (any (+ (set "\t \n\r") -1))
    :ws (some (set "\t "))
    :opt-ws (any (set "\t "))
    :nl-char (+ (* (? "\r") "\n") -1)
    :nl ':nl-char
    :opt-nl (? :nl-char)
    :escape (* "\\" '1)
    :word-span '(some (range "AZ" "az" "09" "--" "__" "  "))
    :anchor-text (* "[" (any (if-not "]" :token)) "]")
    :img-text    (* "![" (any (if-not "]" :token)) "]")
    :anchor-ref  (* "(" '(some (if-not ")" 1)) ")")
    :anchor (/ (* :anchor-text :anchor-ref) ,capture-anchor)
    :img (/ (* :img-text :anchor-ref) ,capture-image)
    :strong ,(line-el "**" "strong")
    :em     ,(line-el "*" "em")
    :trow (* "|"
             (/ (some (* (/ (any :table-token) ,tuple)
                         "|"
                         :opt-ws)) ,tuple)
             :nl)
    :table (/ (* :trow
                 (* (any (if-not :nl-char 1)) :nl-char)
                 (any :trow))
              ,capture-table)
    :code    (* "`" (/ '(any (if-not "`" 1)) ,capture-code) "`")
    :codeblock-inner '(any (if-not "```" 1))
    :codeblock (/ (* "```"
                     '(any (range "AZ" "--" "__" "az" "09")) # capture language
                     :opt-ws :opt-nl
                     :codeblock-inner
                     "```") ,capture-codeblock)
    :table-token (+ :img :anchor :strong
                    :em :code :macro :escape :word-span
                    '(if-not (set "\n|") 1))
    :token (* ':opt-ws (+ :img :anchor :strong
                          :em :code :macro :escape :word-span
                          '(if-not :nl-char 1)))
    :lines (some (* (some :token) :nl))
    :li (* (/ (some :token) ,capture-li) :nl)
    :ulli (* :opt-ws (set "-*") :li)
    :olli (* :opt-ws (some (range "09")) "." :li)
    :ul (* (/ (some :ulli) ,capture-ul) :nl)
    :ol (* (/ (some :olli) ,capture-ol) :nl)
    :paragraph (/ (* :lines :nl-char) ,capture-paragraph)
    :header (/ (* '(between 1 6 "#") (some :token) :nl) ,capture-header)
    :front (* (/ (* '(any (if-not "---" 1)) (argument 0)) ,eval-string) "---" :opt-nl)
    :macro (/ (* "\\" (> 0 "(") :janet-value (argument 0)) ,eval)
    :main (* (+ :front (error "expected front matter"))
             (any (* :next
                     (+ :header :ul :ol
                        :codeblock :table :paragraph -1 (error "")))))})

(def- md-peg
  "A peg that converts markdown to html."
  (peg/compile md-grammar))

(defn md-parse
  "Parse markdown and return a dom."
  [source]
  (def env (make-template-env))
  (def matches (peg/match md-peg source 0 env))
  (unless matches (error "bad markdown"))
  (def front (matches 0))
  (def ret @{:content (tuple/slice matches 1)})
  (when (dictionary? front)
    (loop [[k v] :pairs front]
      (put ret k v)))
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

(defn render
  "Render a document node into HTML. Returns a buffer."
  [node buf state]
  (cond
    (buffer? node) (buffer/push-string buf node)
    (bytes? node) (escape node buf html-escape-chars)
    (indexed? node) (each c node (render c buf state))
    (dictionary? node) (let [tag (node :tag)
                             content (node :content)
                             temp (node :template)
                             no-close (node :no-close)]
                         (when tag
                           (buffer/push-string buf "<" tag)
                           (loop [k :keys node :when (string? k)]
                             (buffer/push-string buf " " k "=\"")
                             (escape (node k) buf attribute-escape-chars)
                             (buffer/push-string buf "\""))
                           (buffer/push-string buf ">"))
                         (if temp
                           ((require-template temp) buf (merge state node))
                           (render content buf state))
                         (when (and tag (not no-close))
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
  (or o (string (string/slice (page :input) 7 -4) ".html")))

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
      :file (when (= ".md" (string/slice path -4))
              (print "Parsing content " path " as markdown...")
              (def page (md-parse (slurp path)))
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
