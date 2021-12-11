###
### syntax/html-syn.janet
### Copyright © 2019 Calvin Rose
###

# A mendoza syntax for highlighting HTML

(import ../syntax :as syntax)

(def grammar
  ~{:ws (any (set " \t\r\n\v"))
    :tagname (/ '(some (+ (range "AZ" "az" "09" "--"))) ,(syntax/span :keyword))
    :open (* '"<" ':ws :tagname :attributes '">")
    :close (* '"</" ':ws :tagname ':ws '">")
    :tag (* :open :root (+ :close (error "missing tag")))
    :no-closers '(+ "br" "meta" "input" "hr" "img" "area" "base" "col"
                    "command" "embed" "keygen" "link" "param" "source"
                    "track" "wbr")
    :singleton (* '"<" ':ws (/ :no-closers ,(syntax/span :keyword)) :attributes '">")
    :openclose (* '"<" ':ws :tagname :attributes '"/>")
    :comment (/ '(* "<!--" (if-not "-->" 1) "-->") ,(syntax/span :comment))
    :html-escape (/ '(* "&" (some (if-not ";" 1)) ";") ,(syntax/span :character))
    :name-start-char (range "::" "AZ" "az" "__")
    :name-char (+ :name-start-char (range "--" ".." "09"))
    :name (/ '(* :name-start-char (any :name-char)) ,(syntax/span :identifier))
    :dblquoted (* "\"" (any (if-not "\"" (+ (* "\\" 1) 1))) "\"")
    :sglquoted (* "'" (any (if-not "'" (+ (* "\\" 1) 1))) "'")
    :string (/ '(+ :dblquoted :sglquoted) ,(syntax/span :string))
    :attribute (* :name '"=" :string)
    :attributes (* (any (* ':ws :attribute)) ':ws)
    :prefix (/ '(* "<!" (any (if-not ">" 1)) ">") ,(syntax/span :comment))
    :normal-text '(some (if-not (set "<&") 1))
    :err (error (/ (* '1 (position))
                   ,(fn [x p] (string "unmatched character "
                                      (describe x)
                                      " at byte " p))))
    :root (any (+ :prefix :normal-text :html-escape :singleton :tag :comment :openclose))
    :main (* ':ws :root (+ -1 :err))})
