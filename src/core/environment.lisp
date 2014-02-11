(in-package :cl-user)
(defpackage cl21.core.environment
  (:use :cl
        #+sbcl :sb-cltl2
        #+openmcl :ccl
        #+cmu :ext
        #+allegro :sys
        #+ecl :si
        #+abcl :lisp)
  #+(or sbcl openmcl cmu allegro ecl abcl)
  (:export :compiler-let
           :variable-information
           :function-information
           :declaration-information
           :augment-environment
           :define-declaration
           :parse-macro
           :enclose))
