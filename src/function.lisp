(in-package :cl-user)
(defpackage cl21.function
  (:use :cl)
  (:import-from :alexandria
                :compose
                :curry
                :rcurry)
  (:export :apply
           :defun
           :fdefinition
           :fboundp
           :fmakunbound
           :flet
           :labels
           :macrolet
           :funcall
           :function
           :function-lambda-expression
           :functionp
           :compiled-function-p
           :call-arguments-limit
           :lambda-list-keywords
           :lambda-parameters-limit

           :compose
           :curry
           :rcurry))
