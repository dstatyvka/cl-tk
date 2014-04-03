(in-package :cl-tk)

(defvar *tk-queue*)
(defvar *tk-thread*)

(defun make-tk-queue ()
  (sb-concurrency:make-mailbox :name "TK GUI"))

(defun push-tk-queue (object)
  (sb-concurrency:send-message *tk-queue* object))

(defun pop-tk-queue ()
  (sb-concurrency:receive-message-no-hang *tk-queue*))

(cffi:defcallback process-tk-queue tcl-result ((event (:pointer (:struct tcl-event))) (flags :int))
  ;; (declare (ignore event flags))
  (format *trace-output* ";;; ~a ~a~&" event flags)
  (handler-case 
      (prog1 :ok
	(let ((funcallable (pop-tk-queue)))
	  (unless (null funcallable)
	    (with-simple-restart (continue "Continue GUI loop")
	      (funcall funcallable)))))
    ;; (serious-condition (condition)
    ;;   (prog1 :error
    ;; 	(%set-result (@interp *tk*) (format nil "~s" condition) (cffi:make-pointer 1))))
    ))

(defun start-gui-loop ()
  (bt:make-thread (lambda ()
                    (cl-tk:with-tk ()
                      (unwind-protect
                           (progn
			     (setf *tk-thread* (tcl-get-current-thread)
				   *tk-queue* (make-tk-queue))
			     (cffi:foreign-funcall "Tk_MainLoop"))
                        (makunbound '*tk-thread*)
                        (makunbound '*tk-queue*))))
                  :name "gui"))

(defun call-in-gui-thread (funcallable)
  (push-tk-queue funcallable)
  (tcl-thread-queue-event *tk-thread* (make-tcl-event (cffi:callback process-tk-queue)) :tail)
  (tcl-thread-alert *tk-thread*))

(defmacro with-gui-thread (() &body body)
  ;; `(call-in-gui-thread (lambda () ,@body))
  (alexandria:with-gensyms (trace-output)
    `(let ((,trace-output *trace-output*))
       (call-in-gui-thread (lambda () (let ((*trace-output* ,trace-output)),@body)))))
  )
