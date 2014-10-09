(in-package :cl-user)

(asdf:defsystem :cl-tk-examples
  :description "Experiments with CL-TK"
  :depends-on (:cl-tk-threads)
  :components ((:module "examples" :components
                        ((:file "text")
                         (:file "tablelist")
			 (:file "canvas")))))

