(in-package :cl-user)
(defpackage cl21.core.environment
  (:use :cl
        #+sbcl :sb-cltl2
        #+openmcl :ccl
        #+cmu :ext
        #+allegro :sys
        #+ecl :si
        #+clasp :clasp-cltl2
        #+abcl :lisp)
  #+(or sbcl openmcl cmu allegro ecl clasp abcl)
  (:export :compiler-let
           :variable-information
           :function-information
           :declaration-information
           :augment-environment
           :define-declaration
           :parse-macro
           :enclose))
