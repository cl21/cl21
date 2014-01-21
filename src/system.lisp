(in-package :cl-user)
(defpackage cl21.system
  (:use :cl)
  (:export :compile-file
           :compile-file-pathname
           :load
           :with-compilation-unit
           :*features*
           :*compile-file-pathname*
           :*compile-file-truename*
           :*load-pathname*
           :*load-truename*
           :*compile-print*
           :*compile-verbose*
           :*load-print*
           :*load-verbose*
           :*modules*
           :provide
           :require))
