###
### mendoza/static.janet
### Copyright © Calvin Rose 2019
###

# This module manages static assets. By default, files
# in static/ are copied into the output directory, but it is
# useful for templates to be able to "register" static assets, so
# that for a template can ensure resources are available, like images
# or css. Registered files must be relative, either to the template
# or to the current working direcctory (project root).

(import ./watch-cache :as wc)

(def- mappings 
  "All static assets will be put into the output site (besides those in static/)."
  @{})

(defn- cp-rf
  "Copy files recursively. Does not copy file permissions, but that's ok for
  sitre assets."
  [src dest]
  (print "Copying " src " to " dest)
  (case (os/stat src :mode)
    :file (spit dest (slurp src))
    :directory (do
                 (os/mkdir dest)
                 (each f (os/dir src)
                   (let [subsrc (string src "/" f)
                         subdest (string dest "/" f)]
                       (cp-rf subsrc subdest))))))

(defn add-file
  "Register a file to be copied into the site/ directory."
  [src dest]
  (def full-src
    (if (string/has-prefix? "." src)
      (let [f (dyn :current-file ".")
            parts (string/split "/" f)
            dirname (if (one? (length parts))
                      "."
                      (string/join (slice parts 0 -2) "/"))]

        (string dirname "/" src))
      (string (os/cwd) "/" src)))
  (put mappings dest full-src))

(defn copy-to-site
  "Move all registered assets into the site directory."
  [site]
  (when (os/stat "static" :mode)
    (cp-rf "static" site))
  (each [dest src] (sort (pairs mappings))
    (cp-rf src (string "./" site "/" dest))))
