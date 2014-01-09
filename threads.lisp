(in-package :cl-tk)

(defvar *tk-queue*)
(defvar *tk-thread*)

(defun make-tk-queue ()
  (sb-concurrency:make-mailbox :name "TK GUI"))

(defun push-tk-queue (object)
  (sb-concurrency:send-message *tk-queue* object))

(defun pop-tk-queue ()
  (sb-concurrency:receive-message-no-hang *tk-queue*))

(cffi:defcallback process-tk-queue :int ((event tcl-event) (flags :int))
  (declare (ignore event flags))
  (let ((funcallable (pop-tk-queue)))
    (when funcallable
      (with-simple-restart (continue "Continue GUI loop")
        (funcall funcallable))
      1)))

(defun start-gui-loop ()
  (bt:make-thread (lambda ()
                    (cl-tk:with-tk ()
                      (unwind-protect
                           (progn (setf *tk-thread* (tcl-get-current-thread)
                                        *tk-queue* (make-tk-queue))
                                  (mainloop))
                        (makunbound '*tk-thread*)
                        (makunbound '*tk-queue*)))))
                  :name "gui"))

(defun call-in-gui-thread (funcallable)
  (push-tk-queue funcallable)
  (tcl-thread-queue-event *tk-thread* (make-tcl-event (cffi:callback process-tk-queue)) :tail)
  (tcl-thread-alert *tk-thread*))

(defmacro with-gui-thread (() &body body)
  `(call-in-gui-thread (lambda () ,@body)))
