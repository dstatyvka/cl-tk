(eval-when (compile eval load)
  (when (ignore-errors (asdf:find-system :cffi))
    (pushnew :cffi *features*)))

(asdf:defsystem :cl-tk-threads
  :description "Minimal bridge to Tcl/Tk"
  :depends-on (#+(and (not allegro) cffi) :cffi #:bordeaux-threads
                 #+sbcl #:sb-concurrency)
  :components ((:file "package")
               (:file "base" :depends-on ("package"))
               (:file "wish" :depends-on ("base"))
               #+(and (not allegro) cffi) (:file "cffi" :depends-on ("base"))
               #+allegro (:file "acl" :depends-on ("base"))
               #+(or cffi allegro) (:file "ffi" :depends-on (#+(and (not allegro) cffi) "cffi" #+allegro "acl"))
               #+sbcl (:file "threads" :depends-on ("cffi"))))
