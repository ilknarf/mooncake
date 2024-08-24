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

(fn add-class-tag [attr fld]
  (let [clname (fld:sub 2)]
    (do
    (if (not (. attr :class))
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
      (while (let [fld (parts)]
               (when fld
                 (add-field res fld)
               fld)))
      res)))

{: page : render-markup : to-base-tag-and-classes : tag-delims}
