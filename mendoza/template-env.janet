###
### mendoza/template-env.janet
### Copyright © Calvin Rose 2019
###

# This file defines functions and macros that are defaultly available
# inside templates.

(defmacro- find-each
  "Iterate like each until the body of the search evaluates to a
  truthy value, and return that."
  [binding ind & body]
  (with-syms [ret]
    ~(do
       (var ,ret nil)
       (each ,binding ,ind
         (set ,ret (do ,;body))
         (if ,ret (break)))
       ,ret)))

(defn dom-find
  "Find the first element in a depth first traversal of the DOM
  that satisfies a given predicate."
  [pred root]
  (cond
    (pred root)
    root
    (dictionary? root)
    (dom-find (root :content))
    (indexed? root)
    (find-each x root (dom-find pred x))))

(defn find-tag
  "Find the first instance of a given tag"
  [tag root]
  (cond
    (dictionary? root)
    (if (= tag (root :tag)) root (find-tag (root :content)))
    (indexed? root)
    (find-each x root (find-tag tag x))))

(defn find-section
  "Find a given section name to insert into the document."
  [name root]
  (dom-find (fn [x] (= (x "name") name)) root))
