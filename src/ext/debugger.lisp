(in-package :cl-user)
(defpackage cl21.ext.debugger
  (:use :cl)
  (:export :invoke-debugger
           :break
           :step ;; NOTE: I'm not sure this function is still useful.
           :*debugger-hook*))
