# Mendoza

Mendoza is a simple static site generator, extensible with and written in the
[Janet](https://janet-lang.org) programming language. Use a simple markup language to author
content and flexible HTML templates for your static content.
Inside templates, use the full power of the [Janet](https://janet-lang.org)
language to generate boilerplate. Mendoza first parses markup into a document,
are then rendered to HTML after going through custom transformations.

## Why?

Mendoza is a tool for authoring content to produce high quality static
HTML websites. It uses the Janet programming language and a derived
markup language to make authoring content fast but flexible. It aims
to be easy to set up straight out of the box with Janet, and used for
authoring documentation for the Janet website.

## Features

* Simple markup language
* Syntax Highlighting
* Extensible with the Janet language - use Janet functions from within markup
* `./mdz watch` - File watching with `inotifywait`
* `./mdz serve` - Serve built static files with circlet
* HTML templating features built in
* No need for a configuration file
* Arbitrary static files

## Usage

First, make sure you have Janet installed. See
[the Janet docs](https://janet-lang.org/introduction.html) for
more information on this. Use the latest version from git, please.

### Global Installation

Run `[sudo] jpm install https://github.com/bakpakin/mendoza.git` script to install on to your machine. The `mdz`
executable will now be on your path, so you can run commands such as `mdz
build` to generate your site in any directory.

### From within this repository

If you want to develop mendoza itself, you need to clone this repository and
then run `[sudo] jpm deps` to install circlet, which mendoza uses to serve
files statically. In this case, use the `./mdz` script to run mendoza.

## License

Mendoza is licensed under the MIT License.
