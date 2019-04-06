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

(sh "cp ./mdz \"" prefix "/bin/mdz\"")
(sh "cp ./mendoza.janet \"" module/*syspath* "\"")
(sh "mkdir -p \"" module/*syspath* "/sublangs\"")
(sh "cp sublangs/janet.janet \"" module/*syspath* "/sublangs\"")
