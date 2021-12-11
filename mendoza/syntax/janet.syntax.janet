###
### syntax/janet-syn.janet
### Copyright © 2019 Calvin Rose
###

# A mendoza syntax for highlighting janet source code.

(import ../syntax :as syntax)

(def- core-env (table/getproto (fiber/getenv (fiber/current))))
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

(defn capture-sym
  [text]
  (def sym (symbol text))
  [(if (or (core-env sym) (specials sym)) :coresym :symbol) text])

(def grammar
  ~{:ws (set " \v\t\r\f\n\0")
    :readermac (set "';~,")
    :symchars (+ (range "09" "AZ" "az" "\x80\xFF") (set "!$%&*+-./:<?=>@^_|"))
    :token (some :symchars)
    :hex (range "09" "af" "AF")
    :escape (* "\\" (+ (set "ntrvzf0e\"\\")
                       (* "x" :hex :hex)
                       (error (constant "bad hex escape"))))
    :comment (/ '(* "#" (any (if-not (+ "\n" -1) 1))) ,(syntax/span :comment))
    :symbol (/ ':token ,capture-sym)
    :keyword (/ '(* ":" (any :symchars)) ,(syntax/span :keyword))
    :constant (/ '(+ "true" "false" "nil") ,(syntax/span :constant))
    :bytes (* "\"" (any (+ :escape (if-not "\"" 1))) "\"")
    :string (/ ':bytes ,(syntax/span :string))
    :buffer (/ '(* "@" :bytes) ,(syntax/span :string))
    :long-bytes {:delim (some "`")
                 :open (capture :delim :n)
                 :close (cmt (* (not (> -1 "`")) (-> :n) ':delim) ,=)
                 :main (drop (* :open (any (if-not :close 1)) :close))}
    :long-string (/ ':long-bytes ,(syntax/span :string))
    :long-buffer (/ '(* "@" :long-bytes) ,(syntax/span :string))
    :number (/ (cmt ':token ,check-number) ,(syntax/span :number))
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
    :dict (* '"@" :struct)
    :main (+ :root (error ""))})
