###
### mendoza/syntax.janet
### Copyright © Calvin Rose 2019
###

# Provides syntax highlighting for janet. 

(def- syntax-classes
  "A set of classes for syntax elements. We want to try and unify
  classes, even between different languages to make highlighting work
  better."
  {:number true
   :keyword true
   :string true
   :coresym true
   :constant true
   :string true
   :character true
   :identifier true
   :comment true
   :operator true
   :type true
   :line true})

(def- language-highlighters
  "Syntax highlighters for all languages. Grammars
   are loaded lazily."
  @{})

(defn span
  "Create a replacer function for a peg grammar that is used to capture
  and color output."
  [class]
  (if (not (syntax-classes class))
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
    (put module/cache (string "mendoza/syntax/" name) nil)
    (put language-highlighters name nil)))

(defn load
  "Load a highlighter if not already loaded. Otherwise, get cached peg."
  [name]
  (if-let [peg (language-highlighters name)]
    peg
    (do
      # The syntax file should call syntax/add on the grammar.
      (print "Loading syntax " name "...")
      (require (string "mendoza/syntax/" name))
      (language-highlighters name))))
