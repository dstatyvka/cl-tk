(in-package :cl-tk)

(defun tcl-int-to-lisp (interp tcl-obj)
  (cffi:with-foreign-object (buffer :int)
    (case (tcl-get-int-from-obj interp tcl-obj buffer)
      (#.+tcl-error+ (tcl-error (get-string-result interp)))
      (#.+tcl-ok+ (cffi:mem-ref buffer :int)))))

(defun tcl-type-name (tcl-obj)
  (let ((type (cffi:mem-aref tcl-obj :pointer 3)))
    (unless (cffi-sys:null-pointer-p type)
      (cffi:foreign-string-to-lisp (cffi:mem-ref type :pointer) :encoding :utf-8))))

(defun tcl-list-to-lisp (interp tcl-obj)
  (cffi:with-foreign-objects ((count :int) (elements :pointer))
    (ecase (tcl-list-obj-get-elements interp tcl-obj count elements)
      (#.+tcl-error+ (tcl-error (get-string-result interp)))
      (#.+tcl-ok+ (loop
		     :with p = (cffi:mem-ref elements :pointer)
		     :for i :from 0 :below (cffi:mem-ref count :int)
		     :collect (tcl-to-lisp interp (cffi:mem-aref p 'tcl-obj i)))))))

(defun tcl-to-lisp (interp tcl-obj &aux (type (tcl-type-name tcl-obj)))
  (cond
    ((or (null type) (string= "string" type))
     (%get-string-from-obj tcl-obj))
    
    ((string= "int" type) (tcl-int-to-lisp interp tcl-obj))
    ((string= "list" type) (tcl-list-to-lisp interp tcl-obj))
    (t (%get-string-from-obj tcl-obj))))

(defun tcl-escape2 (str)
  (with-output-to-string (out)
    (loop :for ch :across str
       :do (princ (case ch
		    (#\newline "\\n") (#\tab "\\t") (#\backspace "\\b")
		    (#\page "\\f") (#\return "\\r") (#\vt "\\v") (#\bell "\\a")
		    ((#\" #\\ #\[ #\] #\$ #\space #\} #\{ #\;) (princ #\\ out) ch)
		    (t ch)) out))))

(defun lisp-to-tcl (value)
  (typecase value
    (string (%new-string-obj  value))
    (keyword (%new-string-obj (format nil "-~(~a~)" value)))
    ((and symbol (not null))
     (let ((tcl-name (get value :tcl-name)))
       (assert (not (null tcl-name)) (tcl-name) "~s has no TCL-name" value)
       (%new-string-obj tcl-name)))
    ((signed-byte 32) (tcl-new-int-obj value))
    (double-float (tcl-new-double-obj value))
    (list (%tcl-new-list-obj value))
    (t (%new-string-obj (prin1-to-string value)))))



(defmethod tcl-send ((tk ffi-tk) command &optional (get-result t) &aux (objc (length command)))
  (with-objv (objv objc command)
    (case (tcl-eval-obj-v (@interp tk) objc objv 0)
      (#.+tcl-error+ (tcl-error (tcl-to-lisp (@interp tk)
					     (tcl-get-obj-result (@interp tk)))))
      (#.+tcl-ok+ (when get-result (tcl-to-lisp (@interp tk)
						(tcl-get-obj-result (@interp tk))))))))

(cffi:defcstruct tcl-token
  (type :int)
  (start :pointer)
  (num-components :int))

(cffi:defcstruct tcl-parse
  (comment-start :pointer)
  (comment-size :int)
  (command-start :pointer)
  (command-size :int)
  (num-words :int)
  (token-ptr :pointer)
  (num-tokens :int)
  (tokens-available :int)
  (error-type :int)
  (begin :pointer)
  (end :pointer)
  (interp :pointer)
  (term :pointer)
  (incomplete :int)
  (tokens (:struct tcl-token) :count 20))

(cffi:defcfun ("Tcl_ParseCommand" tcl-parse-command) :int
  (interp :pointer)
  (start :pointer)
  (num-bytes :int)
  (nested :int)
  (parse-ptr :pointer))


(cffi:defcfun ("Tcl_FreeParse" tcl-free-parse) :int
  (parse-ptr :pointer))

(defun try-parse-command (interp command)
  (cffi:with-foreign-object (parse '(:struct tcl-parse))
    (cffi:with-foreign-string ((string bytes) command)
      (let ((r1 (tcl-parse-command interp string (1- bytes) 0 parse)))
	(unwind-protect
    	     (cffi:with-foreign-slots ((command-size num-words num-tokens) parse
	     			       (:struct tcl-parse))
	       
	       (list r1 command-size num-words num-tokens))
	  (tcl-free-parse parse))))))
