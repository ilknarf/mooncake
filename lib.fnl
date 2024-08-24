;; mutual recursive helpers
(local rec {})

(fn b [str] (str:byte))

(local tag-delims {(b ".") :class (b "#") :id})

;; taken from https://developer.mozilla.org/en-US/docs/Glossary/Void_element
;; fnlfmt: skip
(local void-elements {
                      :area true
                      :base true
                      :br true
                      :col true
                      :embed true
                      :hr true
                      :img true
                      :input true
                      :link true
                      :meta true
                      :param true
                      :source true
                      :track true
                      :wbr true
                      })

(fn string? [v]
  (= (type v) "string"))

(fn table? [v]
  (= (type v) "table"))

;; TODO: remove
;; fnlfmt: skip
(fn page [title]
       [:<>
        [:h1 title]
        [:p "this is the body"]
        [:p :more]
        [:p :more]
        [:p :more]
        [:p :more]
        [:p :more]
        [:p :more]
        [:div
         [:p {:attr {
                     :class "h-3"
                     }} "this is nested"]
         [:p "this is also nested"]]
        [:p :more]
        [:p :more]
        [:p :more]
        [:p :more]
        [:p :more]
        [:p :more]
        [:p :more]])

;; fnlfmt: skip
(local escape-chars {
                    "<" "&lt;"
                    ">" "&gt;"
                    "&" "&amp;"
                    "'" "&#39;"
                    "\"" "&#34;"
                    })

(fn escape-string [str]
  (let [cur []]
  (do
    (for [i 1 (# str)]
      (let [ch (str:sub i i )]
        (table.insert cur (or (. escape-chars ch) ch))))
    (table.concat cur ""))))

;; TODO: something more robust?
;; attr table should be length 0, compared to a markup table
(fn attr-table? [tab]
  (and tab (= 0 (# tab))))

;; need to reference function in mod because of mutual recursion
(fn render-element [tag-data all start]
  (let [cur []
        {:tag raw-tag :attr attr} tag-data
        frag? (or (= raw-tag "<>") (= raw-tag "*"))
        void-el? (. void-elements raw-tag)]
    (do
      (when (= "html" raw-tag)
        (table.insert cur "<!DOCTYPE html"))
      (when (not frag?)
          (let [id (. attr :id)
                class-list (. attr :class)]
            (do
              (var open-tag (.. "<" raw-tag))
              (when id
                (set open-tag (.. open-tag " id=\"" id "\"")))
              (when class-list
                (set open-tag (.. open-tag " class=\"" (table.concat class-list " ") "\"")))
              (set open-tag (.. open-tag ">"))
              (table.insert cur open-tag))))
      (when (not void-el?)
        (for [i start (# all)]
          (let [el (. all i)
                children (if (string? el)
                             [(escape-string el)]
                             (table? el)
                             (rec.render-markup- el)
                             [(tostring el)])]
            (each [_ child (pairs children)]
              (table.insert cur (.. "\t" child))))))
      ;; insert closing tag
      (when (and (not frag?) (not void-el?))
        (table.insert cur (.. "</" raw-tag ">")))
      cur)))

(fn add-class-tag [attr fld]
  (let [clname (fld:sub 2)]
    (do
    (when (not (. attr :class))
        (tset attr :class []))
    (table.insert (. attr :class) clname))))

(fn set-id-tag [attr fld]
  (let [idname (fld:sub 2)]
    (tset attr :id idname)))

(fn add-field [tbl fld]
  (let [fld-symbol (fld:byte 1)
        fld-type (. tag-delims fld-symbol)
        attr (. tbl :attr)]
    (case fld-type
      :class (add-class-tag attr fld)
      :id (set-id-tag attr fld)
      _ (tset tbl :tag fld))))

(fn to-tag-data [tag attr]
  (do
    (let [parts (string.gmatch tag "[\\.#]?[^\\.#]+")
          res {: attr}]
      ;; loop through the iterator, adding all parts to the res table
      (while (let [fld (parts)]
               (when fld
                 (add-field res fld)
                 fld)))
      (if (not (. res :tag))
          (add-field res "div"))
      res)))

(fn render-markup- [{1 tag 2 maybe-attr &as all}]
  (let [has-attr-table? (attr-table? maybe-attr)
        body-start (if has-attr-table? 3 2)
        attr (if has-attr-table?  maybe-attr {})
        tag-data (to-tag-data tag attr)]
    (rec.render-element tag-data all body-start)))

(fn render-markup [body]
  (table.concat (render-markup- body) "\n"))

(tset rec :render-markup- render-markup-)
(tset rec :render-element render-element)

(local mod {: page : render-markup : render-element : to-tag-data : tag-delims})

mod
