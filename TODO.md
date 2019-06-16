# Developer todo list

* One content document -> multiple outputs, or multiple
  content -> one output. Currently, mapping is one -> one.
* Allow html pages in the content directory. The HTML pages would
  be run as templates, where content is nil.
* Some config.janet support. This is less important for now - how
  far can we go without this?
* MathJax support - we don't need to actually include the script
  and CSS in the output HTML, just markdown support for TeX style
  $ and $$.
* Nested lists - might require updating grammar, or departing from
  original markdown syntax. Lists need to capture the indentation
  of the first element, and have subsequent elements match that.
* Better internal linking support. Radio linking, anchors, footnotes, etc.
* Use Janet's custom loaders instead of our own template module system.
* Use custom loaders to load `mdz` files as well.
