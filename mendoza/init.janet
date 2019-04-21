###
### mendoza/init.janet
### Copyright © Calvin Rose 2019
###

(def version "0.0.0")

(import mendoza/markup :as markup)
(import mendoza/render :as render)
(import mendoza/syntax :as syntax)
(import mendoza/template :as template)
(import mendoza/sitemap :as sitemap)

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

  # Make sitemap
  (def smap (sitemap/create pages))

  # Render a page
  (defn render-page
    [page url]
    (def state @{:url url :pages pages :sitemap smap})
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
