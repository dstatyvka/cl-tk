(in-package :cl-tk)

(define-condition tcl-error (simple-error) ())
(defun tcl-error (control &rest args)
  (error 'tcl-error :format-control control :format-arguments args))

(defvar *tk*)

(defclass tk ()
  ())

;; Methods on tk objects

(defgeneric tk-destroy (tk))
(defun destroy () (tk-destroy *tk*))

(defgeneric tcl-send (tk command &optional get-result))

;; Tcl commands

(defun tcl (&rest words)
  (tcl-send *tk* words))

;; Running a Tk instance

(defun start-tk (&optional back-end)
  (or back-end
      (if (find-class 'ffi-tk)
          (handler-case (make-instance 'ffi-tk)
            (error (e) (warn "Failed to start FFI back-end: ~a" (princ-to-string e))
                       (make-instance 'wish-tk)))
          (make-instance 'wish-tk))))

(defmacro with-tk ((&optional back-end) &body body)
  `(let ((*tk* (start-tk ,back-end)))
     (unwind-protect (progn ,@body)
       (destroy))))

(defun toplevel-tk (&optional back-end)
  (setf *tk* (start-tk back-end)))
