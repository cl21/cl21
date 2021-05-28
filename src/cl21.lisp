(defpackage cl21)
(cl:in-package :cl21)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:dolist (#1=#:package-name '(:cl21.core))
    (cl:let ((#2=#:package (cl:find-package #1#)))
      (cl:unless #2#
        (cl:error "Package \"~A\" doesn't exist." #1#))
      (cl:do-external-symbols (#3=#:symbol #2#)
        (cl:shadowing-import (cl:list #3#))
        (cl:export (cl:list #3#))))))

(export-syntax :cl21)

(cl:in-package :cl-user)
(cl21::defpackage cl21-user
  (:use :cl21))
(cl21::in-package :cl21-user)

#+(or sbcl ccl clisp allegro ecl clasp)
(cl:do-external-symbols (#1=#:symb
                         #+sbcl    :sb-ext
                         #+ccl     :ccl
                         #+clisp   :ext
                         #+allegro :excl
                         #+ecl     :quit
                         #+clasp   :ext)
  (cl:shadowing-import (cl:list #1#)))
