###
### mendoza/template-env.janet
### Copyright © Calvin Rose 2019
###

# This file defines functions and macros that are defaultly available
# inside templates.

(import ./static :as static)

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
    (dom-find pred (root :content))
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
  (dom-find (fn [x] (and (dictionary? x) (= (x "name") name))) root))

(defn static-file
  "Make sure a static file is available in the generated site"
  [src &opt dest]
  (default dest src)
  (static/add-file src dest))

(defn relative-url
  "Express a url relative to (dyn :url). The given url should
  be absolute from the site root, like /index.html."
  [url]
  (def b-parts (string/split "/" (dyn :url)))
  (array/remove b-parts -2)
  (def a-parts (string/split "/" url))
  (while (and (not= 0 (length a-parts))
              (not= 0 (length b-parts))
              (= (a-parts 0) (b-parts 0)))
    (array/remove a-parts 0)
    (array/remove b-parts 0))
  (string (string/repeat "../" (length b-parts)) (string/join a-parts "/")))
