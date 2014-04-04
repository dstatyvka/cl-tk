(in-package :cl-user)

(defun in-gui-thread ()
  (cffi:pointer-eq cl-tk::*tk-thread* (cl-tk::tcl-get-current-thread)))

(defmacro define-gui-function (name args &body body)
  (alexandria:with-gensyms (body-closure)
    `(defun ,name ,args
       (flet ((,body-closure () ,@body))
         (if (in-gui-thread)
             (,body-closure)
             (cl-tk:with-gui-thread ()
               (,body-closure)))))))

(define-gui-function tcl! (&rest words)
  (cl-tk:with-gui-thread ()
    (apply #'cl-tk:tcl words)))
