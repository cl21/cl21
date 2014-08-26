(in-package :cl-user)
(defpackage cl21.ext.trace
  (:use :cl)
  (:export :trace
           :untrace
           :*trace-output*))
