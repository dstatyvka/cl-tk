(in-package :cl-user)

(defparameter *canvas* ".canvas")

(defparameter *drag-starts*
  (make-hash-table :test 'equalp))

(cl-tk:defproc (canvas-hit "CanvasHit") (canvas x y)
  (let ((c-x (cl-tk:tcl canvas "canvasx" x))
	(c-y (cl-tk:tcl canvas "canvasy" y)))
    (setf (gethash canvas *drag-starts*)
	  (list c-x c-y (cl-tk:tcl canvas "find" "closest" c-x c-y))))
  "ok")

(cl-tk:defproc (canvas-drag "CanvasDrag") (canvas x y)
  (let ((c-x (cl-tk:tcl canvas "canvasx" x))
	(c-y (cl-tk:tcl canvas "canvasy" y)))
    (destructuring-bind (o-x o-y object)
	(gethash canvas *drag-starts*)
      (cl-tk:tcl canvas "move" object
		 (cl-tk:tcl "expr" c-x "-" o-x)
		 (cl-tk:tcl "expr" c-y "-" o-y))
      (setf (gethash canvas *drag-starts*)
	    (list c-x c-y object))))
  "ok")


(cl-tk:define-gui-function setup-canvas-toplevel (&optional (canvas-name *canvas*))
  (let ((root-children (cl-tk:tcl "winfo" "children" ".")))
    (unless (or (null root-children)
                (and (stringp root-children) (string= "" root-children)))
      (apply #'cl-tk:tcl "destroy" (alexandria:ensure-list root-children))))
  (let* ((vsb (cl-tk:tcl "::ttk::scrollbar" ".vsb" :orient "vertical" :command (list canvas-name "yview")))
	 (hsb (cl-tk:tcl "::ttk::scrollbar" ".hsb" :orient "horizontal" :command (list canvas-name "xview")))
	 (canvas (cl-tk:tcl "canvas" canvas-name
			    :xscrollcommand (list hsb "set")
			    :yscrollcommand (list vsb "set"))))
    (cl-tk:tcl canvas "bind" "movable" "<B1-Motion>" '(canvas-drag "%W" "%x" "%y"))
    (cl-tk:tcl canvas "bind" "movable" "<Button-1>" '(canvas-hit "%W" "%x" "%y"))
    (cl-tk:tcl "grid" canvas :sticky "news" :row 0 :column 0)
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
