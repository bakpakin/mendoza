###
### c.janet
### Copyright © 2019 Calvin Rose
###

# A mendoza syntax for highlighting C

(import mendoza :as mdz)

(defn word-set
  "Create a set of words to match, and return it as a grammar"
  [& words]
  (def with-ws (map (fn [w] ~(* ',w (not :symchar))) words))
  ~(+ ,;with-ws))

(def preproc-words
  (word-set
    "define" "elif" "else" "endif" "error" "if" "ifdef" "ifndefr" "line"
    "pragma" "undef" "warning"))

(def storage-class
  (word-set
    "typedef" "extern" "static" "_Thread_local" "auto" "register"))

(def type-qualifier
  (word-set
    "const" "restrict" "volatile" "_Atomic"))

(def function-spec
  (word-set
    "inline" "_Noreturn"))

(def keywords
  (word-set
    "break" "case" "continue" "default" "do" "else" "for" "goto"
    "if" "return" "sizeof" "switch" "while" "_Alignas" "_Alignof"
    "_Generic" "_Static_assert"))

(def types
  (word-set
    "bool" "char" "double" "enum" "float" "int" "long" "short" "signed"
    "struct" "union" "unsigned" "void" "_Bool" "_Complex" "_Imaginary"
    "ptrdiff_t" "size_t" "ssize_t" "max_align_t" "wchar_t" "intptr_t" "uintptr_t"
    "intmax_t" "uintmax_t" "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t"
    "uint32_t" "int64_t" "uint64_t"))

(def constants
  (word-set
    "NULL" "__DATE__" "__FILE__" "__LINE__" "__TIME__"))

(def grammar
  ~{:ws (set " \v\t\r\f\n\0")
    :wsline (set " \v\t\r\f\0")
    :symchar (range "az" "AZ" "09" "__")
    :line-comment (/ '(* "//" (any (if-not "\n" 1)) "\n") ,(mdz/span :comment))
    :block-comment (/ '(* "/*" (any (if-not "*/" 1)) "*/") ,(mdz/span :comment))
    :comment (+ :line-comment :block-comment)
    :hex (range "09" "af" "AF")
    :escape (* "\\" (+ (set "ntrvz0?ab\"\\")
                       (* "x" :hex :hex)
                       (between 1 3 (range "07"))
                       (* "u" :hex :hex :hex :hex)
                       (error (constant "bad hex escape"))))
    :line-start (+ (not (> -1 1)) (> -1 "\n"))
    :string (/ '(* "\"" (any (+ :escape (if-not "\"" 1))) "\"") ,(mdz/span :string))
    :character (/ '(* "'" (+ :escape (if-not "'" 1)) "'") ,(mdz/span :character))
    :digits (any (range "09"))
    :sign (+ "-" "+" "")
    :float (* :sign :digits (? ".") :digits (+ (* (set "Ee") :digits (? ".") :digits) ""))
    :decimal (* :sign :digits (+ "ULL" "UL" "U" "LL" "L" ""))
    :hexnumber (* :sign "0x" (any :hex))
    :octal (* :sign "0" (any (range "07")))
    :number (/ '(* :hexnumber :octal :decimal :float) ,(mdz/span :number))
    :preproc (/ '(* (any :ws) "#" (some (+ (* "\\" 1) (if-not "\n" 1))))
                ,(mdz/span :line))
    :operator (/ '(set "+-/*%<>~!=^&|?~:;,.()[]{}") ,(mdz/span :operator))
    :root (+  (* :line-start :preproc)
             '(some :wsline)
             '"\n"
             (/ (+ ,keywords ,storage-class ,type-qualifier ,function-spec)
                ,(mdz/span :keyword))
             (/ ,types ,(mdz/span :type))
             (/ ,constants ,(mdz/span :constant))
             (/ '(some :symchar) ,(mdz/span :identifier))
             :string
             :comment
             :number
             :operator
             :character
             -1
             (error ""))
    :main (any :root)})

(mdz/add-syntax "c" grammar)
