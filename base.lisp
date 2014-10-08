(in-package :cl-tk)

(define-condition tcl-error (simple-error) ())
(defun tcl-error (control &rest args)
  (error 'tcl-error :format-control control :format-arguments args))

(defvar *tk*)

;; Methods on tk objects

(defun destroy () (tk-destroy *tk*))

;; Tcl commands

(defun tcl (&rest words)
  (tcl-send *tk* words))

;; Running a Tk instance

(defun start-tk ()
  (init-tcl/tk))

(defmacro with-tk (() &body body)
  `(let ((*tk* (start-tk)))
     (unwind-protect (progn ,@body)
       (destroy))))

(defun toplevel-tk ()
  (setf *tk* (start-tk)))
