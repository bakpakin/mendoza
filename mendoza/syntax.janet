###
### mendoza/syntax.janet
### Copyright © Calvin Rose 2019
###

# Provides syntax highlighting utilities
# for Janet. The majority of the logic is in
# the pegs themselves.

(import mendoza/watch-cache :as wc)

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

(defn add-loader
  "Adds the custom syntax loader to Janet's module/loaders."
  []
  (put module/loaders :mendoza-syntax (fn [x args] 
                                        (print "Loading syntax " x)
                                        (def env ((module/loaders :source) x args))
                                        (def grammar ((env 'grammar) :value))
                                        (unless grammar
                                          (error "module needs to export 'grammar symbol"))
                                        (def peg (if (= :core/peg (type grammar))
                                                   grammar
                                                   (peg/compile grammar)))
                                        (wc/add peg)))
  (def suffix "-syntax.janet")
  (array/insert module/paths 0 [":sys:/mendoza/syntax/:all:" :mendoza-syntax suffix])
  (array/insert module/paths 1 ["./mendoza/syntax/:all:" :mendoza-syntax suffix])
  (array/insert module/paths 2 ["./syntax/:all:" :mendoza-syntax suffix])
  (array/insert module/paths 3 [":all:" :mendoza-syntax suffix]))
