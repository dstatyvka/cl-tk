(in-package #:cl-tk)

(cffi:defctype tcl-command :pointer)

(cffi:defcfun ("Tcl_CreateCommand" %create-command) tcl-command
  (interp :pointer)
  (cmd-name :string)
  (proc :pointer)
  (client-data :pointer)
  (delete-proc :pointer))

(cffi:defcfun ("Tcl_SetResult" %set-result) :void
  (interp :pointer)
  (result :string)
  (free-proc :pointer))

(cffi:defcenum (tcl-result :int)
  (:ok 0)
  (:error 1)
  (:return 2)
  (:break 3)
  (:continue 4))

(defun register-proc (proc-name)
  (%create-command *tk* (get proc-name :tcl-name) (cffi:get-callback proc-name)
		   (cffi-sys:null-pointer)
		   (cffi-sys:null-pointer)))

(defparameter *known-callbacks*
  ())

(defun register-all-procs ()
  (map nil 'register-proc *known-callbacks*))

(defmacro defproc ((name tcl-name &key
			 (interp (gensym "interp"))
			 (client-data (gensym "client-data")))
		   args &body body)
  (alexandria:with-gensyms (argc argv name-idx value-idx option-name option-value result condition)
    (multiple-value-bind (required optionals rest-name keywords)
	(alexandria:parse-ordinary-lambda-list args)
      (let* ((nreqs (length required))
	     (nopts (length optionals))
	     (nkeys (length keywords))
	     (nparams (+ nreqs nopts (* 2 nkeys)))
	     (wrong-num-args 
	      (format nil "wrong # args: ~{~(~a~)~^ ~}~@[ ?~{-~(~a~) value~^ ~}?~]~@[ ?~{~(~a~)~^ ~}?~]"
		      required (mapcar 'caar keywords) (mapcar 'first optionals))))
	(macrolet ((arg (i)
		     ``(cffi:convert-from-foreign (cffi:mem-aref ,argv :pointer ,,i) :string)))
	  `(prog1
	       (eval-when (:load-toplevel :compile-toplevel :execute)
		 (setf (get ',name :tcl-name) ,tcl-name)
		 (pushnew ',name *known-callbacks* :test #'string=)
		 (cffi:defcallback ,name cl-tk::tcl-result
		     ((,client-data :pointer) (,interp :pointer) (,argc :int) (,argv :pointer))
		   (declare (ignorable ,client-data ,argv))
		   (block ,name
		     ,(when (null rest-name)
			    `(unless (<= ,nreqs (1- ,argc) ,nparams)
			       (%set-result ,interp ,wrong-num-args (cffi:make-pointer 1))
			       (return-from ,name :error)))
		     ,(list 'let* (append
				   (when rest-name
				     (list rest-name))
				   (loop :for param :in required
				      :for i :from 1
				      :collect (list param (arg i)))
				   (loop :for (param-name init supplied-p) :in optionals
				      :for i :from (1+ nreqs)
				      :for cond = `(< ,i ,argc)
				      :when supplied-p :collect (list supplied-p cond)
				      :collect (list* 'cl:if (or supplied-p cond) (arg i) init))
				   
				   (loop :for ((kname name) init supplied-p) :in keywords
				      :collect (list name init)
				      :when supplied-p :collect supplied-p))
			    (when keywords
			      `(loop :for ,name-idx :from ,(+ 1 nopts nreqs) :to (min (1- ,argc) ,nparams) :by 2
				  :for ,value-idx = (1+ ,name-idx)
				  :unless (< ,value-idx ,argc)
				  :do (progn (cl-tk::%set-result ,interp "odd number of options" (cffi:make-pointer 1))
					     (return-from ,name :error ))
				  :do (let ((,option-name (string-downcase ,(arg name-idx)))
					    (,option-value ,(arg value-idx)))
					(cond
					  ,@(loop :for ((kname name) init supplied-p) :in keywords
					       :for key-name = (format nil "-~(~a~)" name)
					       :collect `((string= ,option-name ,key-name)
							  (setf ,name ,option-value
								,@(when supplied-p (list supplied-p t)))))
					  (t (cl-tk::%set-result ,interp (format nil "unknown option `~a'" ,option-name)
								 (cffi:make-pointer 1))
					     (return-from ,name :error))))))
			    (when rest-name
			      `(setf ,rest-name (loop :for ,value-idx :from ,(1+ nparams) :to ,argc
						   :collect ,(arg value-idx))))
			    `(handler-case
				 (let ((,result (progn ,@body)))
				   (cl-tk::%set-result ,interp (string ,result) (cffi:make-pointer 1))
				   :ok)
			       (condition (,condition)
				 (cl-tk::%set-result ,interp
						     (format nil "~a" ,condition)
						     (cffi:make-pointer 1))
				 :error))))))
	     (eval-when (:load-toplevel :execute)
	       (handler-case (unless (null *tk-thread*)
			       (with-gui-thread ()
				 (register-proc ',name)))
		 (unbound-variable ())))))))))
