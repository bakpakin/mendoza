(declare-project
  :name "mendoza"
  :author "Calvin Rose"
  :description "A static site generator extensible with and written in Janet."
  :license "MIT"
  :version "0.0.1"
  :url "https://github.com/bakpakin/mendoza"
  :repo "git+https://github.com/bakpakin/mendoza.git"
  :dependencies ["http://github.com/janet-lang/circlet.git"])

(declare-binscript
  :main "mdz"
  :hardcode-syspath true)

(declare-source
  :source ["mendoza"])
