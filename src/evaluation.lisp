(in-package :cl-user)
(defpackage cl21.evaluation
  (:use :cl)
  (:import-from :alexandria
                :once-only)
  (:export :lambda
           :compile
           :eval
           :eval-when
           :load-time-value
           :quote
           :compiler-macro-function
           :define-compiler-macro
           :defmacro
           :macro-function
           :macroexpand
           :macroexpand-1
           :define-symbol-macro
           :symbol-macrolet
           :*macroexpand-hook*
           :proclaim
           :declaim
           :declare
           :ignorable
           :dynamic-extent
           :type
           :notinline
           :ftype
           :declaration
           :optimize
           :special
           :locally
           :the
           :special-operator-p
           :constantp
           :space
           :speed
           :safety
           :debug
           :inline
           :ignore
           :compilation-speed

           :once-only))
