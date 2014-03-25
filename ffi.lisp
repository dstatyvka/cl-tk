(in-package :cl-tk)

(eval-when (compile eval load)
  (defconstant +tcl-ok+ 0)
  (defconstant +tcl-error+ 1)
  (defconstant +tcl-dont-wait+ 2))

(defclass ffi-tk (tk)
  ((interp :reader @interp)
   (alive :initform t :accessor @alive)))

(defmethod initialize-instance :after ((tk ffi-tk) &key &allow-other-keys)
  (format *trace-output* ";; let's create interp ~&")
  (handler-case (load-libs)
    (error (e) (tcl-error (princ-to-string e))))
  (let ((int (create-interp)))
    (when (null-pointer-p int) (tcl-error "Could not create interpreter."))
    (unless (and (= (tcl-init int) +tcl-ok+) (= (tk-init int) +tcl-ok+))
      (tcl-error "Initialising Tcl/Tk failed."))
    (setf (slot-value tk 'interp) int))
  ;; (tcl-send tk "proc _esc {s} {format {\"%s\"} [regsub -all {\"} [regsub -all {\\\\} $s {\\\\\\\\}] {\\\"}]}")
  ;; (tcl-send tk "set _events {}")
  ;; (tcl-send tk "proc _ev {args} {global _events; foreach arg $args {append escaped [_esc $arg]}; lappend _events \"([concat $escaped])\"}")
  ;; (tcl-send tk "proc _get_ev {} {global _events; set ret [lindex $_events 0]; set _events [lrange $_events 1 end]; set ret}")
  (let ((*tk* tk))
    (register-all-procs)
    ;; (tcl-send tk (format nil "bind . <Destroy> {if [string match . %W] {~a}}" (event-handler* (tk-destroy tk))))
    ;; (doevents)
    ))

(defmethod tk-destroy ((tk ffi-tk))
  (when (@alive tk)
    (setf (@alive tk) nil)
    (delete-interp (@interp tk))))

(defmethod tk-alive-p ((tk ffi-tk))
  (@alive tk))

(defmethod tcl-send ((tk ffi-tk) command &optional (get-result t))
  (unless (@alive tk) (tcl-error "Tk instance no longer alive."))
  (case (tcl-eval (@interp tk) command)
    (#.+tcl-error+ (tcl-error (get-string-result (@interp tk))))
    (#.+tcl-ok+ (when get-result (get-string-result (@interp tk))))))

(defmethod tk-doevent ((tk ffi-tk) &optional block)
  (unless (@alive tk) (return-from tk-doevent nil))
  (when (= (do-one-event (if block 0 +tcl-dont-wait+)) 1)
    ;; (loop :for result := (tcl-send tk "_get_ev")
    ;;       :until (string= result "")
    ;;       :do (let ((expr (read-from-string result)))
    ;;             (handle-event tk (parse-integer (car expr)) (cdr expr)))
    ;;       :unless (@alive tk) :do (return))
    t))
