###
### mendoza/sitemap.janet
### Copyright © Calvin Rose 2019
###

# This module constructs a global sitemap
# that is used for creating navigation markup

# Sort pages according to our order
(defn- page-sorter
  [p1 p2]
  (order< [(p1 :order) (p1 :title) (p1 :url)]
          [(p2 :order) (p2 :title) (p2 :url)]))

(defn- insert-page
  [sitemap page frags]
  (var node-parent nil)
  (var node sitemap)
  (each level (tuple/slice frags 0 -2)
    (def next-node (or (node level) @{:fragment level :pages @[]}))
    (put node level next-node)
    (set node-parent node)
    (set node next-node))
  (if (and node-parent (= (last frags) "index.html"))
    (array/push (node-parent :pages) node))
  (array/push (node :pages) page))

(defn- clean-node
  [node]
  (loop [k :keys node :when (string? k)]
    (put node k nil))
  (if-let [pages (node :pages)]
    (each page pages (clean-node page))))

(defn create
  "Create a new SiteMap from a list of pages."
  [pages]
  (sort pages page-sorter)
  (def sitemap @{:fragment "/" :pages @[]})
  (each page pages
    (def frags (string/split "/" (string/slice (page :url) 1)))
    (put page :fragment (last frags))
    (put page :fragments frags)
    (insert-page sitemap page frags))
  (clean-node sitemap)
  sitemap)
