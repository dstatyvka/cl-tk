(in-package :cl-tk)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defconstant +tcl-ok+ 0)
  (defconstant +tcl-error+ 1)
  (defconstant +tcl-dont-wait+ 2))

;; Running a Tk instance

(defun start-tk ()
  (format *trace-output* ";; let's create interp ~&")
  (let ((int (create-interp)))
    (when (cffi:null-pointer-p int) (tcl-error "Could not create interpreter."))
    (unless (and (= (tcl-init int) +tcl-ok+) (= (tk-init int) +tcl-ok+))
      (tcl-error "Initialising Tcl/Tk failed."))
    (let ((*tk* int))
      (register-all-procs))
    int))

(defun destroy (&optional (tk *tk*))
  (unless (cffi:null-pointer-p tk)
    (delete-interp tk)
    (setf tk (cffi:null-pointer))))

(defmacro with-tk (() &body body)
  `(let ((*tk* (start-tk)))
     (unwind-protect (progn ,@body)
       (destroy))))
