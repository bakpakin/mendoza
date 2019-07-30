# Mendoza

Mendoza is a simple static site generator, extensible with and written in the
[Janet](https://janet-lang.org) programming language. Use markdown to author
content and handlebars-like syntax to create templates for your HTML pages.
Inside templates, use the full power of the [Janet](https://janet-lang.org)
language to generate content. Mendoza first parses markup into a document,
are then rendered to HTML after going through custom transformations.

## Why?

Mendoza was created to eventually be of use in the website for
the Janet language - in this regard, it is aimed to be both a tool
for authoring handwritten documentation files, but flexible enough
for other static uses like a blog, slideshow, or personal website
(things I would like to use mendoza for). However, much of this
functionality should be plugged into Mendoza externally. The core
of Mendoza should be fairly simple and dumb, at least in comparison
to similar tools. It should only take a single content file to get
a working site out of the box.

## Features

* Simple markup language
* Syntax Highlighting
* Extensible with the Janet language - use Janet functions from within markup
* `./mdz watch` - File watching with `inotifywait`
* `./mdz serve` - Serve built static files
* HTML templating features built in
* No need for a configuration file
* Arbitrary static files

## Usage

First, make sure you have Janet installed. See
[the Janet docs](https://janet-lang.org/introduction.html) for
more information on this. Use the latest version from git, please.
Once you have installed all of the requirements, clone this
repository.

### Global Installation

Run `[sudo] jpm install` script to install janet on to your machine. The `mdz`
executable will now be on your path, so you can run commands such as `mdz
build` to generate your site in any directory.

### From within this repository

You can also clone this repository and modify it directly to generate
your site. This makes it much easier to hack on the source itself
and extend the language. This also works on all platforms that Mendoza supports.

## License

Mendoza is licensed under the MIT License.
