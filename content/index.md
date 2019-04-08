{
    :title "Mendoza"
    :template "main.html"
}

---

# Mendoza

### A simple static site generator

Mendoza is a simple static site generator, extensible with and written in the
[Janet](https://janet-lang.org) programming language. Use markdown to author
content and handlebars-like syntax to create templates for your HTML pages.
Inside templates, use the full power of the [Janet](https://janet-lang.org)
language to generate content. Mendoza first parses markdown into a document,
which can be manipulated inside templates for even more powerful
templating.

Mendoza was created to eventually be of use in the website for
the Janet language - in this regard, it is aimed to be both a tool
for authoring handwritten documentation files, but flexible enough
for other static uses like a blog, slideshow, or personal website
(things I would like to use mendoza for). However, much of this
functionality should be plugged into Mendoza externally. The core
of Mendoza should be fairly simple and dumb, at least in comparison
to similar tools. It should only take a single content file to get
a working site out of the box.

## Usage

First, make sure you have Janet installed. See
[the Janet docs](https://janet-lang.org/introduction.html) for
more information on this. Use the latest version from git, please.

Run the `./mdz` executable on Linux/posix. On
windows, you will need to run `janet ./mdz`. This should generate
the local documentation for Mendoza, which is of course a Mendoza
static site. Run `./mdz serve` to serve the docs with python locally, or
feel free to use whatever HTTP server you like.

## License

Mendoza is licensed under the MIT License.

## Example Highlighted Janet

```janet
(def a "Hello!")
(print a)
```

```c
#include <stdio.h>
int main()
{
   // printf() displays the string inside quotation
   printf("Hello, World!");
   return 0;
}
```

### Macro example

\(defmacro janet-example
  "Show a janet example in code."
  [example]
  (def result (string/format "%.40p" (eval example)))
  (def no-newlines (string/replace-all "\n " "" result))
  (def code (string/format "%.40p\n# -> %s\n" example no-newlines))
  {:tag "pre" :content {:tag "code" :content code :language "janet"}})

\(janet-example (+ 1 2 3))
