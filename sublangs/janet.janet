###
### janet.janet
### Copyright © 2019 Calvin Rose
###

# A mendoza sublanguage for highlighting janet
# source code. You will need to include
# CSS to highlight this, though.

(def- html-colors
  {:number "mendoza-number"
   :keyword "mendoza-keyword"
   :string "mendoza-string"
   :coresym "mendoza-coresym"
   :constant "mendoza-constant"
   :comment "mendoza-comment"
   :line "mendoza-line"})

(def- escapes
  "Characters to escape for HTML"
  {("&" 0) "&amp;"
   ("<" 0) "&lt;"
   (">" 0) "&gt;"})

(defn- escape
  "Escape a string into buf."
  [str]
  (def buf @"")
  (each byte str
    (if-let [e (escapes byte)]
      (buffer/push-string buf e)
      (buffer/push-byte buf byte)))
  buf)

(defn- paint
  "Paint colors for HTML"
  [what str]
  (def color (get html-colors what))
  (def escaped (escape str))
  (if color
    (string "<span class=\"" color "\">" escaped "</span>")
    escaped))

# Constants for checking if symbols should be
# highlighted.
(def- core-env (table/getproto *env*))
(def- specials {'fn true
               'var true
               'do true
               'while true
               'def true
               'splice true
               'set true
               'unquote true
               'quasiquote true
               'quote true
               'if true})

(defn- check-number [text] (and (scan-number text) text))

(defn- <-c
  "Peg rule for capturing and coloring a rule."
  [color what]
  ~(/ (<- ,what) ,(partial paint color)))

(defn- color-symbol
  "Color a symbol only if it is a core library binding or special."
  [text]
  (def sym (symbol text))
  (def should-color (or (specials sym) (core-env sym)))
  (paint (if should-color :coresym :symbol) text))

(def- janet-grammar
  ~{:ws (set " \v\t\r\f\n\0")
    :readermac (set "';~,")
    :symchars (+ (range "09" "AZ" "az" "\x80\xFF") (set "!$%&*+-./:<?=>@^_|"))
    :token (some :symchars)
    :hex (range "09" "af" "AF")
    :escape (* "\\" (+ (set "ntrvzf0e\"\\")
                       (* "x" :hex :hex)
                       (error (constant "bad hex escape"))))

    :comment ,(<-c :comment ~(* "#" (any (if-not (+ "\n" -1) 1))))

    :symbol (/ ':token ,color-symbol)
    :keyword ,(<-c :keyword ~(* ":" (any :symchars)))
    :constant ,(<-c :constant ~(+ "true" "false" "nil"))
    :bytes (* "\"" (any (+ :escape (if-not "\"" 1))) "\"")
    :string ,(<-c :string :bytes)
    :buffer ,(<-c :string ~(* "@" :bytes))
    :long-bytes {:delim (some "`")
                 :open (capture :delim :n)
                 :close (cmt (* (not (> -1 "`")) (-> :n) ':delim) ,=)
                 :main (drop (* :open (any (if-not :close 1)) :close))}
    :long-string ,(<-c :string :long-bytes)
    :long-buffer ,(<-c :string ~(* "@" :long-bytes))
    :number (/ (cmt ':token ,check-number) ,(partial paint :number))

    :raw-value (+ :comment :constant :number :keyword
                  :string :buffer :long-string :long-buffer
                  :parray :barray :ptuple :btuple :struct :dict :symbol)

    :value (* (? '(some (+ :ws :readermac))) :raw-value '(any :ws))
    :root (any :value)
    :root2 (any (* :value :value))
    :ptuple (* '"(" :root (+ '")" (error "")))
    :btuple (* '"[" :root (+ '"]" (error "")))
    :struct (* '"{" :root2 (+ '"}" (error "")))
    :parray (* '"@" :ptuple)
    :barray (* '"@" :btuple)
    :dict (* '"@"  :struct)

    :main (+ (% (*
                  (constant "<pre class=\"mendoza-main\"><code>")
                  :root
                  (constant "</code></pre>"))) (error ""))})

# Terminal syntax highlighting

(def- peg (peg/compile janet-grammar))

(defn main
  "Highlight janet source code and output HTML."
  [source]
  (buffer (0 (peg/match peg source))))
