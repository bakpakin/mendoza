###
### mendoza/markup-env.janet
### Copyright © Calvin Rose 2019
###

# This file contains declarations for the core
# markup tags available from within a markup file.
# This defines shorthands for many basic HTML tags,
# codeblocks, and more.

(each tag ["ul" "ol" "li" "p" "em" "strong" "u" "pre" "sub" "sup" "tr" "td" "th"]
  (defglobal tag (fn [content] {:tag tag :content content})))

(defn tag
  "Wrap some content in an html tag. If you need attributes or other properties,
  you may want to use raw HTML via the html function."
  [name content]
  {:tag name :content content})

(defn hr
  "Add a horizontal rule"
  []
  {:no-escape "<hr>"})

(defn bigger [content] {:tag "span" "style" "font-size:1.61803398875em;" :content content})
(defn smaller [content] {:tag "span" "style" "font-size:0.61803398875em;" :content content})
(defn code [content] {:tag "code" "class" "mendoza-code" :content content})

(defn anchor
  "Create an in-page anchor for a local link."
  [name & content]
  {:tag "a" "name" name :content content})

(defn codeblock
  "Inline code or codeblock"
  [lang &opt source]
  (def source2 (or source lang))
  (def lang2 (if source lang nil))
  (def highlighter (if lang2 (require (string lang2 ".syntax"))))
  {:tag "pre" 
   "class" "mendoza-codeblock"
   :content {:tag "code"
             :content source2
             :language highlighter
             "data-language" lang2}})
(defn link
  "Create an anchor link"
  [url &opt content]
  {:tag "a" "href" url :content (or content url)})

(defn section
  "Create a section. Usually used to embed different parts of the content
  document into different parts of the main page."
  [name content]
  {:tag "section" "name" name :content content})

(defn blockquote
  "Define a block quote"
  [content]
  {:tag "blockquote" :content content})

(defn image
  [src alt]
  {:tag "img" "src" src :content alt})

(defn center
  "Center some content"
  [content]
  {:tag "div" "class" "mendoza-center" :content content})

(defn html
  "Embed some raw html"
  [source]
  {:no-escape source})

(defn youtube 
  "Add an embedded youtube video in the page"
  [id]
  {:tag "iframe"
   "class" "mendoza-video"
   "width" "560"
   "height" "315"
   "frameborder" "0"
   "allowfullscreen" true
   "align" "center"
   "src" (string "//www.youtube.com/embed/" id)})

(defn vimeo
  "Add an embedded vimeo video in the page"
  [id]
  {:tag "iframe"
   "class" "mendoza-video"
   "width" "560"
   "height" "315"
   "frameborder" "0"
   "allowfullscreen" true
   "align" "center"
   "src" (string "//player.vimeo.com/video/" id)})

(defn gist
  "Embed a github gist"
  [user-and-id]
  {:tag "script"
   "src" (string "https://gist.github.com/" user-and-id ".js")})

(defn asciinema
  "Embed an asciinema video into your site"
  [id & args]
  (def prs (mapcat (fn [[k v]] [(string "data-" k) v]) (partition 2 args)))
  (def basetbl (table ;prs))
  {:tag "div" "class" "asciinema-wrap"
   :content 
   (merge basetbl {:tag "script"
                   "src" (string "https://asciinema.org/a/" id ".js")
                   "id" (string "asciicast-" id)
                   "async" true})})

(import ./data/insta :as insta)
(defn instagram
  "Add an instagram link in the page"
  [id]
  {:tag "div"
   "class" "mendoza-video"
   :content {:no-escape (string/replace "!!!!!" (string id) insta/html)}})

# Documentation generation

(def- docstring-peg-source
  "Convert a docstring into a dom node."
  ~{:ws (set " \t\r\n\0\f")
    :funcdef (* (any :ws) 
                (/ '(* "(" (any (if-not ")" 1)) ")")
                  ,|(codeblock "janet" $))
                "\n\n")
    :br (* "\n\n" (constant {:tag "br" :no-close true}))
    :li (* "\t" (/ '(any (if-not "\n" 1)) ,|{:tag "li" :content $}))
    :ul (* (some :li) (+ "\n" -1))
    :sent '(some (if-not "\n" 1))
    :main (* (? :funcdef) (/ (group (any (+ :ul :sent :br "\n"))) ,|{:tag "p" :content $}))})

(def- docstring-peg (peg/compile docstring-peg-source))

(defn- emit-item
  "Generate documentation for one entry."
  [key env-entry]
  (let [{:macro macro
         :value val
         :ref ref
         :source-map sm
         :doc docstring} env-entry
        real-val (if ref (get ref 0) val)
        binding-type (cond
                       macro :macro
                       ref (string :var " (" (type real-val) ")")
                       (type val))
        full-binding-type (cond
                            macro binding-type
                            (and (nil? real-val) (not ref)) binding-type
                            (function? real-val) binding-type
                            (cfunction? real-val) binding-type
                            (and (bytes? real-val) (< 35 (length real-val))) binding-type
                            [binding-type " " {:tag "code" "class" "binding-realval" :content (describe real-val)}])
        source-ref (if-let [[path line col] sm]
                     {:tag "div" "class" "source-map" :content (string path " at line " line ", column " col)}
                     "")
        doc2 (or docstring "")
        doc-dom (peg/match docstring-peg doc2)]
    {:tag "div" "class" "docstring" :content
     [{:tag "div" "class" "binding-wrap"
       :content [{:tag "span" "class" "binding" :content {:tag "a" "id" key :content (string key)}}
                 {:tag "span" "class" "binding-type" :content full-binding-type}
                 source-ref]}
      doc-dom]}))

(defn api-docs
  "Generate docs for a given module. Returns a node."
  [module &opt prefix]
  (def env (if (string? module) (require module) module))
  (seq [[k entry]
        :in (sort (pairs env))
        :when (symbol? k)
        :when (or (not prefix) (string/has-prefix? prefix k))
        :when (and (get entry :doc) (not (get entry :private)))]
       (emit-item k entry)))

(defn api-index
  "Generate an index for the given docs."
  [module &opt prefix]
  (def env (if (string? module) (require module) module))
  (def items (seq [[k entry]
        :in (sort (pairs env))
        :when (symbol? k)
        :when (or (not prefix) (string/has-prefix? prefix k))
        :when (and (get entry :doc) (not (get entry :private)))]
    {:tag "a" "href" (string "#" k) :content (string k)}))              
  {:tag "p" :content (interpose {:tag "span" :content " " "class" "divider"} items)})
