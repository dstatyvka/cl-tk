(in-package :cl-user)

(asdf:defsystem :cl-tk-examples
  :description "Experiments with CL-TK"
  :depends-on (:cl-tk-threads :cxml)
  :components ((:module "examples" :components
                        ((:file "common")
                         (:file "text" :depends-on ("common"))
                         (:file "tablelist" :depends-on ("common"))))))

