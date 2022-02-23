###
### mendoza/init.janet
### Copyright © Calvin Rose 2019
###

(def version "0.0.1")

(import ./markup :as markup)
(import ./render :as render)
(import ./syntax :as syntax)
(import ./template :as template)
(import ./sitemap :as sitemap)
(import ./static :as static)
(import ./watch-cache :as watch-cache)

# For serving local files
(import circlet)

#
# Add loaders
#

(defn init
  "Add loaders to environment. Call this before running
  any commands."
  []
  (syntax/add-loader)
  (template/add-loader)
  (markup/add-loader))

#
# File System Helpers
#

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
  [lead page]
  (def front-trim (length lead))
  (def o (page :url))
  (or o (string (string/slice (page :input) front-trim -5) ".html")))

(defn- rimraf
  "Remove a directory and all sub directories."
  [path]
  (if-let [m (os/stat path :mode)]
    (if (= m :directory)
      (do
        (each subpath (os/dir path) (rimraf (string path "/" subpath)))
        (os/rmdir path))
      (os/rm path))))

(defn- url-prefix
  "Make sure URL has leading slash."
  [url]
  (if (= ("/" 0) (url 0))
    url
    (string (dyn :site-root "/") url)))

#
# Main API
#

(defn clean
  "Clean up the old site."
  [&opt site]
  (default site "site")
  (unless (= site ".")
    (print "Removing directory " site "...")
    (rimraf site))
  (print "Unloading cached modules...")
  (watch-cache/clean))

(defn serve
  "Serve the site locally."
  [&opt port host site]
  (default port 8000)
  (default host "127.0.0.1")
  (default site "site")
  (let [port ((if (string? port) scan-number identity) port)]
    (circlet/server
      (->
        {:default {:kind :static
                   :root site}}
        circlet/router
        circlet/logger)
      port host)))

# re-export render
(setdyn 'render (dyn 'render/render))

(defn build
  "Build the static site and put it in the output folder."
  [&opt site root]

  (default site "site")
  (default root "/")

  (setdyn :site-root root)

  # Clean up old artifacts
  (clean site)
  (os/mkdir site)

  # Read in pages
  (def pages @[])
  (defn read-pages [root &opt path]
    (default path root)
    (case (os/stat path :mode)
      :directory (each f (sort (os/dir path))
                   (read-pages root (string path "/" f)))
      :file (when (and (> (length path) 3) (= ".mdz" (string/slice path -5)))
              (print "Parsing content " path " as mendoza markup")
              (def page (require path))
              (put page :input path)
              (put page :url (page-get-url root page))
              (array/push pages page))))
  (read-pages "content")
  (read-pages "doc")

  # Make sitemap
  (def smap (sitemap/create pages))

  # Render a page
  (defn render-page
    [page url]
    (def out
      (with-dyns [:url url
                  :pages pages
                  :sitemap smap
                  :page page]
        (loop [[k v] :pairs page :when (keyword? k)]
          (setdyn k v))
        (render/render page @"")))
    (def outpath (string site url))
    (print "Writing HTML to " outpath)
    (create-dirs outpath)
    (spit outpath out))

  # Render all pages
  (loop [page :in pages]
    (def url (url-prefix (page :url)))
    (render-page page url)
    (if-let [permalinks (page :permalinks)]
      (each link permalinks
        (render-page page (url-prefix link)))))

  # Copy static stuff
  (static/copy-to-site site))

(defn watch
  "Watch for files changing, and re-run mendoza when source files
  change. Only works when content files and templates change, and
  only on linux for now."
  []

  # Check which directories exist
  (def watched-dirs @[])
  (each path ["static" "templates" "syntax" "mendoza/syntax" "content" "doc"]
    (if (os/stat path :mode)
      (array/push watched-dirs path)))

  (defn rebuild []
    (def f (fiber/new build :e))
    (def res (resume f))
    (case (fiber/status f)
      :error (do
               (:write stdout "build ")
               (:flush stdout)
               (debug/stacktrace f res))))
  (rebuild)
  # Detect if inotify is present
  (defn command-possible [command]
    # watch runs on linux and mac at least
    # detection is not a solution for windows
    # fswatch technically can run on windows however
    (def proc (os/spawn ["which" command] :p {:out :pipe}))
    (zero? (:wait proc))
  # Choose command
  (def cmd
    (cond
      (command-possible "inotifywait") ["inotifywait" "-m" "-r" ;watched-dirs "-e" "modify"]
      (command-possible "fswatch") ["fswatch" "-r" "--one-per-batch" ;watched-dirs]
      (error "inotifywait, fswatch not available, cannot perform watch.")))
  (def proc (os/spawn cmd :p {:out :pipe}))
  (if (not (zero? (:wait proc))) (error (string "could not run " (describe cmd))))
  (while true
    (print "Waiting...")
    (def x (:read (proc :out) :line))
    (if (or (not x) (empty? x)) (break))
    (print "Event: " x)
    (rebuild)))
