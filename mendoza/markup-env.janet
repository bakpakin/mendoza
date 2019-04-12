###
### mendoza/markup-env.janet
### Copyright © Calvin Rose 2019
###

# This file contains declarations for the core
# markup tags available from within a markup file.
# This defines shorthands for many basic HTML tags,
# codeblocks, and more.

(each tag ["ul" "ol" "li" "p" "em" "strong" "u" "pre" "sub" "sup"]
  (defglobal tag (fn [content] {:tag tag :content content})))
(defn bigger [content] {:tag "span" "style" "font-size:1.61803398875em;" :content content})
(defn smaller [content] {:tag "span" "style" "font-size:0.61803398875em;" :content content})
(defn code [content] {:tag "code" :content content})
(defn codeblock
  "Inline code or codeblock"
  [lang &opt source]
  (def source2 (or source lang))
  (def lang2 (if source lang nil))
  {:tag "pre" :content {:tag "code"
                        :content source2
                        :language lang2
                        "data-language" lang2}})
(defn link
  "Create an anchor link"
  [url content]
  {:tag "a" "href" url :content content})

(defn blockquote
  "Define a block quote"
  [content]
  {:tag "blockquote" :content content})

(defn image
  [src alt]
  {:tag "img" "src" src :content alt})
