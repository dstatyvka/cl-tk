(in-package :cl-user)

(defparameter *canvas* ".canvas")

(cl-tk:define-gui-function setup-canvas-toplevel (&optional (canvas-name *canvas*))
  (let ((root-children (cl-tk:tcl "winfo" "children" ".")))
    (unless (or (null root-children)
                (and (stringp root-children) (string= "" root-children)))
      (apply #'cl-tk:tcl "destroy" (alexandria:ensure-list root-children))))
  (let ((vsb (cl-tk:tcl "::ttk::scrollbar" ".vsb" :orient "vertical" :command (list canvas-name "yview")))
        (hsb (cl-tk:tcl "::ttk::scrollbar" ".hsb" :orient "horizontal" :command (list canvas-name "xview"))))
    (cl-tk:tcl "grid" (cl-tk:tcl "canvas" canvas-name
                                 :xscrollcommand (list hsb "set")
                                 :yscrollcommand (list vsb "set"))
               :sticky "news" :row 0 :column 0)
    (cl-tk:tcl "grid" vsb :sticky "ns" :row 0 :column 1)
    (cl-tk:tcl "grid" hsb :sticky "ew" :row 1 :column 0)
    (cl-tk:tcl "grid" "columnconfigure" "." 0 :weight 1)
    (cl-tk:tcl "grid" "rowconfigure" "." 0 :weight 1)))

(cl-tk:define-gui-function canvas-update-scrolling ()
  (cl-tk:tcl *canvas* "configure" :scrollregion (cl-tk:tcl *canvas* "bbox" "all")))

(cl-tk:define-gui-function canvas-create-rect (x1 y1 x2 y2 &rest args)
  (apply #'cl-tk:tcl *canvas* "create" "rectangle" (list x1 y1 x2 y2) args))

(cl-tk:define-gui-function canvas-create-oval (x1 y1 x2 y2 &rest args)
  (apply #'cl-tk:tcl *canvas* "create" "oval" (list x1 y1 x2 y2) args))

(cl-tk:define-gui-function canvas-create-line (coords &rest args)
  (apply #'cl-tk:tcl *canvas* "create" "line" coords args))

(cl-tk:define-gui-function canvas-create-text (x y text &rest args)
  (apply #'cl-tk:tcl *canvas* "create" "text" x y :text text args))

(cl-tk:define-gui-function canvas-clear ()
  (cl-tk:tcl *canvas* "delete" "all"))
