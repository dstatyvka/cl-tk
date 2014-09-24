(in-package :cl-user)

(asdf:defsystem :cl-tk-threads
  :description "Minimal bridge to Tcl/Tk"
  :depends-on (#:cffi #:bordeaux-threads #+sbcl #:sb-concurrency)
  :components ((:file "package")
               (:file "base" :depends-on ("package"))
               (:file "cffi" :depends-on ("base"))
	       (:file "proc" :depends-on ("cffi"))
	       (:file "ffi" :depends-on ("cffi"))
	       (:file "cffi-ext" :depends-on ("ffi"))
               #+sbcl (:file "threads" :depends-on ("cffi"))))
