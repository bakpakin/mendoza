###
### mendoza/template-env.janet
### Copyright © Calvin Rose 2019
###

# This file defines functions and macros that are defaultly available
# inside templates.

(defn find-tag
  "Find the first instance of a given tag"
  [tag root]
  (cond
    (dictionary? root)
    (if (= tag (root :tag)) root (find-tag (root :content)))
    (indexed? root)
    (some (partial find-tag tag) root)))
