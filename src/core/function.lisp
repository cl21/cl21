(in-package :cl-user)
(defpackage cl21.core.function
  (:use :cl)
  (:shadow :function)
  (:import-from :alexandria
                :compose
                :conjoin
                :disjoin
                :curry
                :rcurry
                :define-constant)
  (:export
   :lambda
   :lm
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
   :compose
   :conjoin
   :disjoin
   :curry
   :rcurry

   ;; Variables
   :+call-arguments-limit+
   :+lambda-list-keywords+
   :+lambda-parameters-limit+))
(in-package :cl21.core.function)

(define-constant +call-arguments-limit+
  call-arguments-limit
  :documentation #.(documentation 'call-arguments-limit 'variable))

(define-constant +lambda-list-keywords+
  lambda-list-keywords
  :documentation #.(documentation 'lambda-list-keywords 'variable)
  :test #'equal)

(define-constant +lambda-parameters-limit+
  lambda-parameters-limit
  :documentation #.(documentation 'lambda-parameters-limit 'variable))


(deftype function () 'cl:function)

(defmacro function (name-or-form)
  (if (atom name-or-form)
      `(cl:function ,name-or-form)
      (destructuring-bind (head . rest) name-or-form
        (if (eq head 'setf)
            `(cl:function ,name-or-form)
            `(,(ecase head
                      (and 'conjoin)
                      (or 'disjoin)
                      (not 'complement)
                      (compose 'compose)
                      ((cl:lambda function cl:function lm
                                  #+sbcl sb-int:named-lambda)
                       head))
               ,@(if (member head '(and or not compose) :test #'eq)
                     (mapcar (lambda (x) `(function ,x)) rest)
                     rest))))))

(defmacro lm (args &body body)
  "A shorthand for LAMBDA.

Currently this cannot be used at a function position."
  `(lambda ,args ,@body))
