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


(cffi:defctype tcl-obj :pointer)

(defun tcl-incr-ref-count (obj-ptr)
  (incf (cffi:mem-ref obj-ptr :int)))

(defun tcl-decr-ref-count (obj-ptr)
  (decf (cffi:mem-ref obj-ptr :int)))

(cffi:defcfun ("Tcl_NewObj" tcl-new-obj) tcl-obj)

(cffi:defcfun ("Tcl_NewStringObj" tcl-new-string-obj) tcl-obj
  (utf8-bytes :pointer)
  (length :int))

(cffi:defcfun ("Tcl_GetStringFromObj" tcl-get-string-from-obj ) :pointer
  (obj-ptr tcl-obj)
  (length :pointer))

(defun %get-string-from-obj (obj)
  (cffi:with-foreign-object (length :int)
    (let ((p (tcl-get-string-from-obj obj length)))
      (unless (cffi-sys:null-pointer-p p)
	(cffi:foreign-string-to-lisp p :count (cffi:mem-ref length :int) :encoding :utf-8)))))

(defun %new-string-obj (string)
  (cffi:with-foreign-string ((pointer length) string :encoding :utf-8)
    (tcl-new-string-obj pointer (1- length))))

(cffi:defcfun ("Tcl_NewListObj" tcl-new-list-obj) tcl-obj
  (objc :int)
  (objv :pointer))

(defmacro with-objv ((buffer-var length-var list) &body body)
  (let ((i (gensym)) (v (gensym)))
    `(cffi:with-foreign-object (,buffer-var 'tcl-obj ,length-var)
       (loop
	  :for ,i :from 0 
	  :for ,v :in ,list
	  :do (setf (cffi:mem-aref ,buffer-var 'tcl-obj ,i)
		    (lisp-to-tcl ,v)))
       ,@body)))

(defun %tcl-new-list-obj (list &aux (n (length list)))
  (with-objv (buffer n list)
    (tcl-new-list-obj n buffer)))

(cffi:defcfun ("Tcl_NewIntObj" tcl-new-int-obj) tcl-obj
  (value :int))

(defun tcl-escape2 (str)
  (with-output-to-string (out)
    (loop :for ch :across str
       :do (princ (case ch
		    (#\newline "\\n") (#\tab "\\t") (#\backspace "\\b")
		    (#\page "\\f") (#\return "\\r") (#\vt "\\v") (#\bell "\\a")
		    ((#\" #\\ #\[ #\] #\$ #\space #\} #\{ #\;) (princ #\\ out) ch)
		    (t ch)) out))))

(defun lisp-to-tcl (value)
  (etypecase value
    (string (%new-string-obj (tcl-escape2 value)))
    (keyword (%new-string-obj (format nil "-~(~a~)" value)))
    ((signed-byte 32) (tcl-new-int-obj value))
    (list (%tcl-new-list-obj value))))

(defun tcl-type-name (tcl-obj)
  (let ((type (cffi:mem-aref tcl-obj :pointer 3)))
    (unless (cffi-sys:null-pointer-p type)
      (cffi:foreign-string-to-lisp (cffi:mem-ref type :pointer) :encoding :utf-8))))

(cffi:defcfun ("Tcl_GetIntFromObj" tcl-get-int-from-obj) :int
  (interp :pointer)
  (tcl-obj tcl-obj)
  (intPtr :pointer))

(defun tcl-int-to-lisp (interp tcl-obj)
  (cffi:with-foreign-object (buffer :int)
    (case (tcl-get-int-from-obj interp tcl-obj buffer)
      (#.+tcl-error+ (tcl-error (get-string-result interp)))
      (#.+tcl-ok+ (cffi:mem-ref buffer :int)))))

;; (cffi:defcfun ("Tcl_GetObjType"))

(defun tcl-to-lisp (interp tcl-obj &aux (type (tcl-type-name tcl-obj)))
  (cond
    ((or (null type) (string= "string" type))
     (%get-string-from-obj tcl-obj))
    
    ((string= "int" type) (tcl-int-to-lisp interp tcl-obj))
    ((string= "list" type) (tcl-list-to-lisp interp tcl-obj))
    (t (%get-string-from-obj tcl-obj))))

(cffi:defcfun ("Tcl_ListObjGetElements" tcl-list-obj-get-elements) :int
  (interp :pointer)
  (listPtr tcl-obj)
  (objcPtr :pointer)
  (objvPtr :pointer))

(defun tcl-list-to-lisp (interp tcl-obj)
  (cffi:with-foreign-objects ((count :int) (elements :pointer))
    (ecase (tcl-list-obj-get-elements interp tcl-obj count elements)
      (#.+tcl-error+ (tcl-error (get-string-result interp)))
      (#.+tcl-ok+ (loop
		     :with p = (cffi:mem-ref elements :pointer)
		     :for i :from 0 :below (cffi:mem-ref count :int)
		     :collect (tcl-to-lisp interp (cffi:mem-aref p 'tcl-obj i)))))))

(cffi:defcfun ("Tcl_EvalObjEx" tcl-eval-obj) :int
  (interp :pointer)
  (obj-ptr tcl-obj)
  (flags :int))

(cffi:defcfun ("Tcl_EvalObjv" tcl-eval-obj-v) :int
  (interp :pointer)
  (objc :int)
  (objv :pointer)
  (flags :int))

(cffi:defcfun ("Tcl_GetObjResult" tcl-get-obj-result) tcl-obj
  (interp :pointer))

(defmethod tcl-send-2 ((tk ffi-tk) command &optional (get-result t) &aux (objc (length command)))
  (unless (@alive tk) (tcl-error "Tk instance no longer alive."))
  (with-objv (objv objc command)
    (case (tcl-eval-obj-v (@interp tk) objc objv 0)
      (#.+tcl-error+ (tcl-error (tcl-to-lisp (@interp tk)
					     (tcl-get-obj-result (@interp tk)))))
      (#.+tcl-ok+ (when get-result (tcl-to-lisp (@interp tk)
						(tcl-get-obj-result (@interp tk))))))))