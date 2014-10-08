(in-package :cl-tk)

(define-condition tcl-error (simple-error) ())
(defun tcl-error (control &rest args)
  (error 'tcl-error :format-control control :format-arguments args))

(defvar *tk*)

(defclass tk ()
  ())

;; Methods on tk objects

(defgeneric tk-destroy (tk))
(defgeneric tk-alive-p (tk))
(defun destroy () (tk-destroy *tk*))
(defun alive-p () (tk-alive-p *tk*))

(defgeneric tk-doevent (tk &optional block))
(defun doevent (&optional block) (tk-doevent *tk* block))
(defun doevents () (loop :while (doevent)))
(defun mainloop ()
  (loop :while (alive-p)
        :do (doevent t)))

(defgeneric tcl-send (tk command &optional get-result))

;; Tcl commands

(defun tcl-escape (str)
  (if (string= str "")
      "{}"
      (with-output-to-string (out)
        (loop :for ch :across str
              :do (princ (case ch
                           (#\newline "\\n") (#\tab "\\t") (#\backspace "\\b")
                           (#\page "\\f") (#\return "\\r") (#\vt "\\v") (#\bell "\\a")
                           ((#\" #\\ #\[ #\] #\$ #\space #\} #\{ #\;) (princ #\\ out) ch)
                           (t ch)) out)))))

(defstruct (literal-string (:constructor lit (val))) val)

(defun tcl (&rest words)
  (tcl-send-2 *tk* words))

;; wnames

(defun wname-cons (name base)
  (format nil "~a.~a" (if (string= base ".") "" base) name))
(flet ((find-dot (name)
         (or (position #\. name :from-end t)
             (tcl-error "~a is not a valid wname" name))))
  (defun wname-car (name)
    (subseq name (1+ (find-dot name))))
  (defun wname-cdr (name)
    (subseq name 0 (max 1 (find-dot name)))))

(defvar *wname* ".")
(defmacro with-wname (name &body body)
  `(let ((*wname* ,name)) ,@body))
(defun wname (name &optional id)
  (wname-cons (if id (format nil "~a~a" name id) name) *wname*))

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
