(in-package :cl-tk)

(cffi:define-foreign-library tcl
  (:darwin (:framework "Tcl"))
  (:windows (:or "tcl85.dll"))
  (:unix (:or "libtcl8.5.so" "libtcl.so"))
  (t (:default "libtcl")))

(cffi:define-foreign-library tk
  (:darwin (:framework "Tk"))
  (:windows (:or "tk85.dll"))
  (:unix (:or "libtk8.5.so" "libtk.so"))
  (t (:default "libtk")))

(let ((loaded nil))
  (defun load-libs ()
    (unless loaded
      (cffi:use-foreign-library tcl)
      (cffi:use-foreign-library tk)
      (setf loaded t))))

(cffi:defcfun ("Tcl_CreateInterp" create-interp) :pointer)
(cffi:defcfun ("Tcl_DeleteInterp" delete-interp) :void (interp :pointer))
(cffi:defcfun ("Tcl_Init" tcl-init) :int (interp :pointer))
(cffi:defcfun ("Tcl_Eval" tcl-eval) :int (interp :pointer) (script (:string :encoding :utf-8)))
(cffi:defcfun ("Tcl_GetStringResult" get-string-result) (:string :encoding :utf-8) (interp :pointer))
(cffi:defcfun ("Tcl_DoOneEvent" do-one-event) :int (flags :int))
(cffi:defcfun ("Tk_Init" tk-init) :int (interp :pointer))

(defun null-pointer-p (ptr) (cffi:null-pointer-p ptr))

(cffi:defcfun ("Tcl_Alloc" tcl-alloc) :pointer (size :uint))

(cffi:defcenum tcl-queue-position
  :tail :head :mark)

(cffi:defctype tcl-thread :pointer)

(cffi:defcstruct tcl-event
  (proc :pointer)
  (next :pointer))

(cffi:defcfun ("Tcl_GetCurrentThread" tcl-get-current-thread) tcl-thread)

(cffi:defcfun ("Tcl_ThreadQueueEvent" tcl-thread-queue-event)  :void
  (thread tcl-thread)
  (event (:pointer (:struct tcl-event)))
  (position tcl-queue-position))

(defun make-tcl-event (callback)
  (let ((tcl-event (tcl-alloc (cffi:foreign-type-size '(:struct tcl-event)))))
    (cffi:with-foreign-slots ((proc next) tcl-event (:struct tcl-event))
      (setf proc callback
	    next (cffi:null-pointer)))
    tcl-event))

(cffi:defcfun ("Tcl_ThreadAlert" tcl-thread-alert) :void
  (thread tcl-thread))
