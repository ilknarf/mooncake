;; fnlfmt: skip
(fn b [str] (: str :byte))

(local tag-delims {(b ".") :class (b "#") :id})

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

(fn render-markup [{1 tag 2 maybe-attr &as all}])

(fn render-element [tag-data all start]
  (let [cur []
        {:tag raw-tag :attr attr} tag-data
        frag? (or (= raw-tag "<>") (= raw-tag "*"))]
    (do
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
      (for [i start (+ (# all) 1)]
        (let [el (. all i)]
          (print)))
      ;; insert closing tag
      (when (not frag?)
        (table.insert cur (.. "</" raw-tag ">")))
      (table.concat cur ""))))

;; TODO: something more robust?
;; attr table should be length 0, compared to a markup table
(fn attr-table? [tab]
  (= 0 (# tab)))

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

(fn to-base-tag-and-classes [tag attr]
  (do
    (let [parts (string.gmatch tag "[\\.#]?[^\\.#]+")
          res {: attr}]
      ;; loop through the iterator, adding all parts to the res table
      (while (let [fld (parts)]
               (when fld
                 (add-field res fld)
               fld)))
      res)))

{: page : render-markup : render-element : to-base-tag-and-classes : tag-delims}
