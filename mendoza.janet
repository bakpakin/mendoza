###
### mendoza.janet
### Copyright © 2019 Calvin Rose
###

(def version "0.0.0")

#
# Markdown Sub-Languages
#

(def- sublangs
  "Cached sublanguage pipes."
  @{})

(defn- get-sublang
  "Get a sublanguage pipe from the language name."
  [name]
  (if-let [pipe (sublangs name)]
    pipe
    (let [env (require (string "sublangs/" name))
          pipe ((env 'main) :value)]
      (put sublangs name pipe)
      pipe)))

#
# Markdown -> DOM parser
#

(defn- combine-strings
  "Flatten consecutive strings in an indexed structure by concatenating them.
  The resulting document graph should be much cleaner. Also, empty strings will
  be removed entirely."
  [els]
  (let [buf @""
        accum @[]
        flush (fn []
                (unless (empty? buf)
                  (array/push accum (string buf))
                  (buffer/clear buf)))]
    (loop [x :in els]
      (if (bytes? x)
        (buffer/push-string buf x)
        (do (flush) (array/push accum x))))
    (flush)
    (tuple/slice accum)))

(defn- capture-tokens
  "Wrapper to make combine-strings variadic and useful as a subtitution function
  in the peg grammar."
  [& args]
  (combine-strings args))

(defn- capture-header
  "Peg capture function for parsing markdown headers."
  [prefix & content]
  (def n (length prefix))
  {:tag (string "h" n)
   :content (combine-strings content)})

(defn- capture-anchor
  "Peg capture function for parsing markdown links."
  [& args]
  {:tag "a"
   :content (combine-strings (tuple/slice args 0 -2))
   "href" (last args)})

(defn- capture-image
  "Peg capture function for parsing markdown images."
  [& args]
  {:tag "img"
   :content (combine-strings (tuple/slice args 0 -2))
   :no-close true
   "src" (last args)})

(defn- line-el
  "Create a peg pattern for parsing some markup
  on a line. Captures the HTML text to output."
  [delim tag]
  (defn replace
    [& args]
    {:tag tag
     :content (combine-strings args)})
  ~(* ,delim
      (/ (any (if-not ,delim :token)) ,replace)
      ,delim))

(defn- capture-table
  "Peg capture function for a table"
  [& contents]
  {:tag "table"
   :content (combine-strings contents)})

(defn- capture-code
  "Peg capture function for single line of code"
  [code]
  {:tag "code" :content [code]})

(defn- capture-codeblock
  "Peg capture function for multiline codeblock"
  [language code]
  (if (= "" language)
    {:tag "pre" :content code}
    ((get-sublang language) code)))

(defn- capture-li
  "Capture a list inside the peg and create a Document Node."
  [& contents]
  {:tag "li"
   :content (combine-strings contents)})

(defn- capture-ol
  "Capture a list inside the peg and create a Document Node."
  [& contents]
  {:tag "ol"
   :content (combine-strings contents)})

(defn- capture-ul
  "Capture a list inside the peg and create a Document Node."
  [& contents]
  {:tag "ul"
   :content (combine-strings contents)})

(defn- capture-paragraph
  "Capture a paragraph node in the peg."
  [& contents]
  {:tag "p"
   :content (combine-strings contents)})

(def- md-grammar
  "Grammar for markdown -> document AST parser."
  ~{:next (any (+ (set "\t \n\r") -1))
    :ws (some (set "\t "))
    :opt-ws (any (set "\t "))
    :nl-char (+ (* (? "\r") "\n") -1)
    :nl ':nl-char
    :opt-nl (? :nl-char)
    :escape (* "\\" '1)
    :anchor-text (* "[" (any (if-not "]" :token)) "]")
    :img-text    (* "![" (any (if-not "]" :token)) "]")
    :anchor-ref  (* "(" '(some (if-not ")" 1)) ")")
    :anchor (/ (* :anchor-text :anchor-ref) ,capture-anchor)
    :img (/ (* :img-text :anchor-ref) ,capture-image)
    :strong ,(line-el "**" "strong")
    :em     ,(line-el "*" "em")
    :trow (* "|"
             (/ (some (* (/ (any :table-token) ,capture-tokens)
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
                     '(any (range "AZ" "--" "__" "az")) # capture language
                     :opt-ws :opt-nl
                     :codeblock-inner
                     "```") ,capture-codeblock)
    :table-token (+ :img :anchor :strong
                    :em :code :escape '(if-not (set "\n|") 1))
    :token (* ':opt-ws (+ :img :anchor :strong
                          :em :code :escape '(if-not :nl-char 1)))
    :lines (some (* (some :token) :nl))
    :li (* (/ (some :token) ,capture-li) :nl)
    :ulli (* :opt-ws (set "-*") :li)
    :olli (* :opt-ws (some (range "09")) "." :li)
    :ul (* (/ (some :ulli) ,capture-ul) :nl)
    :ol (* (/ (some :olli) ,capture-ol) :nl)
    :paragraph (/ (* :lines :nl-char) ,capture-paragraph)
    :header (/ (* '(between 1 6 "#") (some :token) :nl) ,capture-header)
    :front (* '(any (if-not "---" 1)) "---" :opt-nl)
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
  (def matches (peg/match md-peg source))
  (unless matches (error "bad markdown"))
  (def front (eval-string (matches 0)))
  (def ret @{:tag "div"
             :content (tuple/slice matches 1)})
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
  [root &opt buf]
  (default buf @"")
  (defn render1
    "Render a node"
    [node]
    (cond
      (buffer? node) (buffer/push-string buf node)
      (bytes? node) (escape node buf html-escape-chars)
      (indexed? node) (each c node (render1 c))
      (dictionary? node) (let [tag (node :tag)
                               content (node :content)
                               no-close (node :no-close)]
                           (buffer/push-string buf "<" tag)
                           (loop [k :keys node :when (string? k)]
                             (buffer/push-string buf " " k "=\"")
                             (escape (node k) buf attribute-escape-chars)
                             (buffer/push-string buf "\""))
                           (buffer/push-string buf ">")
                           (render1 content)
                           (unless no-close (buffer/push-string buf "</" tag ">")))
      (escape (string node) buf html-escape-chars)))
  (render1 root)
  buf)

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
  "Compile a template string into a function"
  [source &opt where]

  (default where (string source))
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
      (string " (render " str " " bufsym ") ")))

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
  (def ast ~(fn _template [content pages url]
              (def ,bufsym @"")
              ,;forms
              ,bufsym))

  (def ctor (compile ast env where))
  (if-not (function? ctor)
    (error (string "could not compile template: " (string/format "%p" ctor))))
  (ctor))

#
# Content Functions -> traverse the DOM
#

(defn content-search-tag
  "Search the dom for the first occurence of a tag. Return the
   content of that dom element. Returns nil if no tag found."
  [tag dom]
  (when (and tag (not (bytes? tag)))
    (if (indexed? dom)
      (some (partial content-search-tag tag) dom)
      (or (and (= (dom :tag) tag) (dom :content))
          (content-search-tag tag (dom :content))))))

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

(def- default-template
  "Default template for documents if no template is specified."
  (template
`````<!doctype html>
<html>
<head>
  <meta charset="utf-8">
  <title>{{ (content :title) }}</title>
  <style type="text/css">
.mendoza-main { color: white; background: #111; }
.mendoza-number { color: #89dc76; }
.mendoza-keyword { color: #ffd866; }
.mendoza-string { color: #ab90f2; }
.mendoza-coresym { color: #ff6188; }
.mendoza-constant { color: #fc9867; }
.mendoza-comment { color: darkgray; }
.mendoza-line { color: gray; }
  </style>
</head>
<body>
{{ content }}
</body>
</html>
`````))

(defn- page-get-template
  "Get the template for a dom"
  [templates page]
  (def t (page :template))
  (or (and t (templates t)) default-template))

(defn- page-get-url
  "Get the output url for a dom"
  [page]
  (def o (page :url))
  (or o (string (string/slice (page :input) 8 -4) ".html")))

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

  # Read in templates
  (def templates @{})
  (when (os/stat "templates" :mode)
    (each f (os/dir "templates")
      (def tpath (string "templates/" f))
      (print "Parsing template " tpath " as bar template...")
      (put templates f (template (slurp tpath) tpath))))

  # Render a page
  (defn render-page
    [page url]
    (def out ((page-get-template templates page)
              page
              pages
              url))
    (def outpath (string "site/" url))
    (print "Writing HTML to " outpath "...")
    (create-dirs outpath)
    (spit outpath out))

  # Render all pages
  (loop [page :in pages]
    (def url (page :url))
    (if (indexed? url)
      (each u url (render-page page u))
      (render-page page url))))
