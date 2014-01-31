(in-package :cl21-user)
(defpackage cl21-test.package
  (:use :cl21
        :cl-test-more))
(in-package :cl21-test.package)

(plan 3)

(defpackage foo.bar.baz
  (:use :cl21)
  (:export :hello))
(in-package :foo.bar.baz)

(defun hello () "Hello!")

(in-package :cl21-test.package)

(is (foo.bar.baz:hello) "Hello!")

(add-package-local-nickname :fbz :foo.bar.baz)

(is (foo.bar.baz:hello) "Hello!")
(is (fbz:hello) "Hello!")

(finalize)
