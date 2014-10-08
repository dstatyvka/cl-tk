(in-package :cl-tk)

(cffi:define-foreign-library tcl
  (:darwin (:framework "Tcl"))
  (:windows (:or "tcl85.dll"))
  (:unix (:or "libtcl8.5.so" "libtcl.so"))
  (t (:default "libtcl")))

(cffi:define-foreign-library tk
  (:darwin (:framework "Tk"))
  (:windows (:or "tk85.dll"))
  (:unix (:or "libtk8.5.so" "libtk.so"))
  (t (:default "libtk")))
