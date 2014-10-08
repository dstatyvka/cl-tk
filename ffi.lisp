(in-package :cl-tk)

(eval-when (compile eval load)
  (defconstant +tcl-ok+ 0)
  (defconstant +tcl-error+ 1)
  (defconstant +tcl-dont-wait+ 2))

(defclass ffi-tk (tk)
  ((interp :reader @interp)
   (alive :initform t :accessor @alive)))

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
  (when (@alive tk)
    (setf (@alive tk) nil)
    (delete-interp (@interp tk))))

(defmethod tk-alive-p ((tk ffi-tk))
  (@alive tk))

(defmethod tcl-send ((tk ffi-tk) command &optional (get-result t))
  (unless (@alive tk) (tcl-error "Tk instance no longer alive."))
  (case (tcl-eval (@interp tk) command)
    (#.+tcl-error+ (tcl-error (get-string-result (@interp tk))))
    (#.+tcl-ok+ (when get-result (get-string-result (@interp tk))))))

(defmethod tk-doevent ((tk ffi-tk) &optional block)
  (unless (@alive tk) (return-from tk-doevent nil))
  (when (= (do-one-event (if block 0 +tcl-dont-wait+)) 1)
    t))
