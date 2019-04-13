###
### mendoza/render.janet
### Copyright © Calvin Rose 2019
###

(import mendoza/template :as template)
(import mendoza/syntax :as syntax)

(def- html-escape-chars
  "Characters to escape for HTML"
  {("&" 0) "&amp;"
   ("<" 0) "&lt;"
   (">" 0) "&gt;"})

(def- attribute-escape-chars
  "Characters to escape for HTML"
  {("\"" 0) "&quot;"
   ("'" 0) "&#39;"})

(defn- escape
  "Escape a string into buf."
  [str buf escapes]
  (each byte str
    (if-let [e (escapes byte)]
      (buffer/push-string buf e)
      (buffer/push-byte buf byte))))

(defn- highlight-genhtml
  "Paint syntax highlighting colors for HTML"
  [buf tokens colors]
  (each token tokens
    (if (bytes? token)
      (escape token buf html-escape-chars)
      (let [[class bytes] token
            color (colors class)]
        (if color
          (do
            (buffer/push-string buf "<span style=\"color:" color "\">")
            (escape bytes buf html-escape-chars)
            (buffer/push-string buf "</span>"))
          (escape bytes buf html-escape-chars))))))

(defn render
  "Render a document node into HTML. Returns a buffer."
  [node buf state]
  (cond
    (bytes? node) (if (state :no-escape)
                    (buffer/push-string buf node)
                    (escape node buf html-escape-chars))
    (indexed? node) (each c node (render c buf state))
    (dictionary? node)
    (let [next-state (merge state node)
          tag (node :tag)]
      (when tag
        (buffer/push-string buf "<" tag)
        (loop [k :keys node :when (string? k)]
          (buffer/push-string buf " " k "=\"")
          (escape (node k) buf attribute-escape-chars)
          (buffer/push-string buf "\""))
        (buffer/push-string buf ">"))
      (if-let [lang (node :language)]
        (let [content (render (node :content) @"" next-state)
              matches (peg/match (syntax/load lang) content)
              colors (or (next-state :colors) syntax/default-colors)]
          (highlight-genhtml buf matches colors))
        (if-let [temp (node :template)]
          ((template/load temp) buf next-state render)
          (render (node :content) buf next-state)))
      (when (and tag (not (node :no-close)))
        (buffer/push-string buf "</" tag ">")))
    (number? node) (buffer/push-string buf (string node)))
  buf)
