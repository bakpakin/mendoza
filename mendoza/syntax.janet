###
### mendoza/syntax.janet
### Copyright © Calvin Rose 2019
###

# Provides syntax highlighting for janet. 

(def default-colors
  "Default theme colors for syntax highlighting."
  {:number "#89dc76"
   :keyword "#ffd866"
   :string "#ab90f2"
   :coresym "#ff6188"
   :constant "#fc9867"
   :string "#fc9867"
   :character "red"
   :identifier "white"
   :comment "gray"
   :operator "white"
   :type "green"
   :line "gray"})

(def- language-highlighters
  "Syntax highlighters for all languages. Grammars
   are loaded lazily."
  @{})

(defn span
  "Create a replacer function for a peg grammar that is used to capture
  and color output."
  [class]
  (if (not (default-colors class))
    (error (string "invalid class " class))
    (fn [text] [class text])))

(defn add
  "Define a grammar for syntax highlighting. This just registers a
  grammar that will be used for a given language."
  [name grammar]
  (def peg (if (= :core/peg (type grammar)) grammar (peg/compile grammar)))
  (put language-highlighters name peg)
  nil)

(defn unload
  "Unload all loaded syntax highlighters."
  []
  (loop [name :keys language-highlighters]
    (put module/cache (string "syntax/" name) nil)
    (put language-highlighters name nil)))

(defn load
  "Load a highlighter if not already loaded. Otherwise, get cached peg."
  [name]
  (if-let [peg (language-highlighters name)]
    peg
    (do
      # The syntax file should call syntax/add on the grammar.
      (print "Loading syntax " name "...")
      (require (string "syntax/" name))
      (language-highlighters name))))
