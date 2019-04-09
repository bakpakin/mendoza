# Developer todo list

* Block macros - macros that operate on blocks of markdown
  document, not just Janet values.
* One content document -> multiple outputs, or multiple
  content -> one output. Currently, mapping is one -> one.
* Direct syntactic support for partials - use of a template
  embedded inside a content document. Similar useage to block
  macros, but templates can be written in HTML instead of janet.
* Allow partials inside templates - easily allow templates to
  require other templates. This of course can be done with
  janet code inside the template, but should have first class
  support.
* Allow html pages in the content directory. The HTML pages would
  be run as templates, where content is nil.
* Some config.janet support. This is less important for now - how
  far can we goo without this?
* MathJax support - we don't need to actually include the script
  and CSS in the output HTML, just markdown support for TeX style
  $ and $$.
