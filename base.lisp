(in-package :cl-tk)

(define-condition tcl-error (simple-error) ())
(defun tcl-error (control &rest args)
  (error 'tcl-error :format-control control :format-arguments args))

(defvar *tk*)

;; Tcl commands

(defun tcl (&rest words)
  (tcl-send *tk* words))
