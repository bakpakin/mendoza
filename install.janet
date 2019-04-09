# Install mendoza globally.

(def prefix (or (os/getenv "PREFIX") "/usr/local"))
(defn sh
  "Run shell command."
  [& args]
  (def cmd (string ;args))
  (print cmd)
  (let [status (os/shell cmd)]
    (if (zero? status) true
      (error (string "command exited with status " status)))))

(sh "cp ./mdz \"" prefix "/bin/\"")
(sh "cp ./mendoza.janet \"" module/*syspath* "\"")
(sh "mkdir -p \"" module/*syspath* "/sublangs\"")
(sh "cp -r syntax \"" module/*syspath* "/syntax\"")
