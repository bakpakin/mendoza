###
### mendoza/init.janet
### Copyright © Calvin Rose 2019
###

(def version "0.0.0")

(import mendoza/markup :as markup)
(import mendoza/render :as render)
(import mendoza/syntax :as syntax)
(import mendoza/template :as template)

#
# File System Helpers
#

(defn- cp-rf
  "Copy files recursively. Does not copy file permissions, but that's ok for
  a static site."
  [src dest]
  (os/mkdir dest)
  (each f (os/dir src)
    (let [subsrc (string src "/" f)
          subdest (string dest "/" f)]
      (if (= (os/stat subsrc :mode) :directory)
        (cp-rf subsrc subdest)
        (spit subdest (slurp subsrc))))))

(defn- create-dirs
  "Recursively create directories for a path if they don't exist"
  [url]
  (def parts (tuple/slice (string/split "/" url) 0 -2))
  (def buf @"")
  (each part parts
    (buffer/push-string buf part)
    (def path (string buf))
    (unless (= (os/stat path :mode) :directory)
      (os/mkdir path))
    (buffer/push-string buf "/")))

(defn- page-get-url
  "Get the output url for a dom"
  [page]
  (def o (page :url))
  (or o (string (string/slice (page :input) 7 -5) ".html")))

(defn- rimraf
  "Remove a directory and all sub directories."
  [path]
  (if-let [m (os/stat path :mode)]
    (if (= m :directory)
      (do
        (each subpath (os/dir path) (rimraf (string path "/" subpath)))
        (os/rmdir path))
      (os/rm path))))

(defn- insert-into
  "Insert into a nested table with a list of keys. Creates
  subtables as needed."
  [ds value & ks]
  (var tab ds)
  (for i 0 (- (length ks) 1)
    (def k (ks i))
    (if-let [subtab (tab k)]
      (set tab subtab)
      (let [newtab @{}]
        (put tab k newtab)
        (set tab newtab))))
  (put tab (last ks) value))

(defn- keymap
  "Map function over keys and values in a map."
  [tab f]
  (def newtab @{})
  (loop [k :keys tab]
    (put newtab k (f k (tab k))))
  newtab)

#
# Main API
#

(defn clean
  "Clean up the old site."
  []
  (print "Removing directory site...")
  (rimraf "site")
  (print "Unloading templates...")
  (template/unload)
  (print "Unloading syntaxes...")
  (syntax/unload))

(defn serve
  "Serve the site locally."
  [&opt port]
  (default port "8000")
  (os/shell (string "cd site; python3 -m http.server " port)))

(defn build
  "Build the static site and put it in the output folder."
  []

  # Clean up old artifacts
  (clean)
  (os/mkdir "site")

  # Copy static stuff
  (when (os/stat "static" :mode)
    (cp-rf "static" "site"))

  # Read in pages
  (def pages @[])
  (defn read-pages [path]
    (case (os/stat path :mode)
      :directory (each f (sort (os/dir path))
                   (read-pages (string path "/" f)))
      :file (when (= ".mdz" (string/slice path -5))
              (print "Parsing content " path " as mendoza markup")
              (def page (markup/markup (slurp path)))
              (put page :input path)
              (put page :url (page-get-url page))
              (array/push pages page))))
  (read-pages "content")

  # Construct page tree
  (def root @{})
  (each page pages
    (def frags (string/split "/" (string/slice (page :url) 1)))
    (put page :fragment (last frags))
    (put page :fragments frags)
    (insert-into root page ;frags))

  # For ordering children
  (defn kid-sort-rep
    [k]
    [(if-let [page (k :page)] (page :order))
     (if-let [page (k :page)] (page :title))
     (k :name)])
  (defn order-kids
    [k1 k2]
    (order< (kid-sort-rep k1) (kid-sort-rep k2)))

  # Contruct TOC
  (defn make-toc
    [[name root siblings]]
    (def kids (map make-toc (seq [k :keys root :when (string? k)] [k (root k) root])))
    (sort kids order-kids)
    (if (empty? kids)
      (do (put root :siblings siblings) {:name (or (root :title) name) :page root})
      {:name name :kids kids}))
  (def toc ((make-toc ["/" root nil]) :kids))

  # Render a page
  (defn render-page
    [page url]
    (def state @{:url url :pages pages :root root :toc toc :siblings (page :siblings)})
    (def out (render/render page @"" state))
    (def outpath (string "site" url))
    (print "Writing HTML to " outpath "...")
    (create-dirs outpath)
    (spit outpath out))

  # Render all pages
  (loop [page :in pages]
    (def url (page :url))
    (render-page page url)
    (if-let [permalinks (page :permalinks)]
      (each link permalinks
        (render-page page link)))))

(defn watch
  "Watch for files changing, and re-run mendoza when source files
  change. Only works when content files and templates change, and
  only on linux for now."
  []

  # Check which directories exist
  (def watched-dirs @"")
  (each path ["static" "templates" "syntax" "content"]
    (if (os/stat path :mode)
      (buffer/push-string watched-dirs path " ")))

  (defn rebuild []
    (def f (fiber/new build :e))
    (def res (resume f))
    (case (fiber/status f)
      :error (do
               (:write stdout "build ")
               (:flush stdout)
               (debug/stacktrace f res))))
  (rebuild)
  (def cmd (string "inotifywait -m -r " watched-dirs "-e modify"))
  (def proc (file/popen cmd :r))
  (if (not proc) (error "could not run " (describe cmd)))
  (while true
    (print "waiting...")
    (def x (:read proc :line))
    (if (or (not x) (empty? x)) (break))
    (print "event: " x)
    (rebuild))
  (file/close proc))
