(in-package :cl-tk)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defconstant +tcl-ok+ 0)
  (defconstant +tcl-error+ 1)
  (defconstant +tcl-dont-wait+ 2))

(defclass ffi-tk (tk)
  ((interp :accessor @interp)))

(defmethod initialize-instance :after ((tk ffi-tk) &key &allow-other-keys)
  (format *trace-output* ";; let's create interp ~&")
  (let ((int (create-interp)))
    (when (null-pointer-p int) (tcl-error "Could not create interpreter."))
    (unless (and (= (tcl-init int) +tcl-ok+) (= (tk-init int) +tcl-ok+))
      (tcl-error "Initialising Tcl/Tk failed."))
    (setf (slot-value tk 'interp) int))
  (let ((*tk* tk))
    (register-all-procs)))

(defmethod tk-destroy ((tk ffi-tk))
  (unless (cffi:null-pointer-p (@interp tk))
    (delete-interp (@interp tk))
    (setf (@interp tk) (cffi:null-pointer))))
