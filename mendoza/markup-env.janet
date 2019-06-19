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
  {:no-escape true :content "<hr>"})

(defn bigger [content] {:tag "span" "style" "font-size:1.61803398875em;" :content content})
(defn smaller [content] {:tag "span" "style" "font-size:0.61803398875em;" :content content})
(defn code [content] {:tag "code" "class" "mendoza-code" :content content})

(defn codeblock
  "Inline code or codeblock"
  [lang &opt source]
  (def source2 (or source lang))
  (def lang2 (if source lang nil))
  {:tag "pre" 
   "class" "mendoza-codeblock"
   :content {:tag "code"
             :content source2
             :no-escape (not (not lang2))
             :language lang2
             "data-language" lang2}})
(defn link
  "Create an anchor link"
  [url content]
  {:tag "a" "href" url :content content})

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
  {:no-escape true :content source})

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
  (merge basetbl {:tag "script"
                  "src" (string "https://asciinema.org/a/" id ".js")
                  "id" (string "asciicast-" id)
                  "async" true}))

(import ./data/insta)
(defn instagram
  "Add an instagram link in the page"
  [id]
  {:tag "div"
   "class" "mendoza-video"
   :content {:no-escape true
             :content (string/replace "!!!!!" (string id) ./data/insta/html)}})
