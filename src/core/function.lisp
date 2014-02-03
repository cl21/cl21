(in-package :cl-user)
(defpackage cl21.core.function
  (:use :cl)
  (:shadow :function)
  (:import-from :alexandria
                :compose
                :conjoin
                :disjoin
                :curry
                :rcurry)
  (:export
   :lambda
   :apply
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
   :conjoin
   :disjoin
   :curry
   :rcurry))
(in-package :cl21.core.function)

(deftype function () 'cl:function)

(defmacro function (name-or-form)
  (if (atom name-or-form)
      `(cl:function ,name-or-form)
      `(,(ecase (car name-or-form)
           (and 'conjoin)
           (or 'disjoin)
           (not 'complement)
           (compose 'compose)
           ((cl:lambda
             function cl:function
             #+sbcl sb-int:named-lambda) (car name-or-form)))
        ,@(if (member (car name-or-form) '(and or not compose) :test #'eq)
              (mapcar (lambda (x) `(function ,x))
                      (cdr name-or-form))
              (cdr name-or-form)))))
