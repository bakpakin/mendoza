###
### mendoza/syntax.janet
### Copyright © Calvin Rose 2019
###

# Provides syntax highlighting utilities
# for Janet. The majority of the logic is in
# the pegs themselves.

(import ./watch-cache :as wc)

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

(defn span
  "Create a replacer function for a peg grammar that is used to capture
  and color output."
  [class]
  (if (not (syntax-classes class))
    (error (string "invalid class " class))
    (fn [text] [class text])))

#
# Module loading
#

(def- syntax-dir (module/expand-path "" ":cur:/syntax"))
(def- suffix ".syntax")
(defn add-loader
  "Adds the custom syntax loader to Janet's module/loaders."
  []
  (defn loader [x args] 
    (print "Loading syntax " x)
    (def env ((module/loaders :source) x args))
    (def grammar ((env 'grammar) :value))
    (unless grammar
      (error "module needs to export 'grammar symbol"))
    (def peg (if (= :core/peg (type grammar))
               grammar
               (peg/compile grammar)))
    (wc/add peg))
  (put module/loaders :mendoza-syntax loader)
  (array/push
    module/paths
    [(string syntax-dir "/:all:.janet") :mendoza-syntax suffix]
    ["./syntax/:all:.janet" :mendoza-syntax suffix]))
