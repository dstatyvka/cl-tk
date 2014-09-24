(eval-when (compile eval load)
  (when (ignore-errors (asdf:find-system :cffi))
    (pushnew :cffi *features*)))

(asdf:defsystem :cl-tk-threads
  :description "Minimal bridge to Tcl/Tk"
  :depends-on (#+(and (not allegro) cffi) :cffi #:bordeaux-threads
                 #+sbcl #:sb-concurrency)
  :components ((:file "package")
               (:file "base" :depends-on ("package"))
               #+(and (not allegro) cffi) (:file "cffi" :depends-on ("base"))
	       #+(and (not allegro) cffi) (:file "proc" :depends-on ("cffi"))
               #+allegro (:file "acl" :depends-on ("base"))
               #+(or cffi allegro) (:file "ffi" :depends-on (#+(and (not allegro) cffi) "cffi" #+allegro "acl"))
	       (:file "cffi-ext" :depends-on ("ffi"))
               #+sbcl (:file "threads" :depends-on ("cffi"))))
