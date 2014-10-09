(in-package :cl-user)

(cl-tk:define-gui-function show-data-table (table-list-widget &key titles data)
  (loop
     :initially
     (cl-tk:tcl table-list-widget "delete" "0" "end")
     (cl-tk:tcl "catch" (list table-list-widget "deletecolumns" "0" "end"))
     (cl-tk:tcl table-list-widget "configure" "-columntitles"
                (apply 'cl-tk:tcl "list" titles))
     :for row :in data
     :do (cl-tk:tcl table-list-widget "insert" "end" row)))

(cl-tk:define-gui-function show-plist (table-list-widget row-format data)
  (loop
     :initially
     (cl-tk:tcl table-list-widget "delete" "0" "end")
     (cl-tk:tcl "catch" (list table-list-widget "deletecolumns" "0" "end"))
     (loop :for (indicator caption) :in row-format
        :do (cl-tk:tcl table-list-widget "insertcolumns" "end" "0" caption))
     
     :for row :in data
     :do (loop
            :for (indicator caption) :in row-format
            :for value = (getf row indicator)
            :collect (format nil "~a" value) :into cells
            :finally (cl-tk:tcl table-list-widget "insert" "end" cells))))

(cl-tk:define-gui-function setup-tablelist-toplevel (&optional (table-list-name ".table"))
  (cl-tk:tcl "package" "require" "tablelist")
  (let ((root-children (cl-tk:tcl "winfo" "children" ".")))
    (unless (null root-children)
      (apply #'cl-tk:tcl "destroy" root-children)))
  (let ((vsb (cl-tk:tcl "::ttk::scrollbar" ".vsb" :orient "vertical" :command (list table-list-name "yview")))
        (hsb (cl-tk:tcl "::ttk::scrollbar" ".hsb" :orient "horizontal" :command (list table-list-name "xview"))))
    (cl-tk:tcl "grid" (cl-tk:tcl "::tablelist::tablelist" table-list-name
                                 :xscrollcommand (list hsb "set")
                                 :yscrollcommand (list vsb "set"))
               :sticky "news" :row 0 :column 0)
    (cl-tk:tcl "grid" vsb :sticky "ns" :row 0 :column 1)
    (cl-tk:tcl "grid" hsb :sticky "ew" :row 1 :column 0)
    (cl-tk:tcl "grid" "columnconfigure" "." 0 :weight 1)
    (cl-tk:tcl "grid" "rowconfigure" "." 0 :weight 1)))
