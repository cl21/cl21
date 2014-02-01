(in-package :cl21-user)
(defpackage cl21-test.package
  (:use :cl21
        :cl-test-more))
(in-package :cl21-test.package)

(plan 14)

(defpackage foo.bar.baz
  (:use :cl21)
  (:export :hello))
(in-package :foo.bar.baz)

(defun hello () "Hello!")

(in-package :cl21-test.package)

(is (foo.bar.baz:hello) "Hello!")

(add-package-local-nickname :foo-bar-baz :foo.bar.baz)

(is (foo.bar.baz:hello) "Hello!")
(is (foo-bar-baz:hello) "Hello!")

(defpackage test-package
  (:use :cl21
        (:foo.bar.baz :as :fbz))
  (:export :test-hello))
(in-package :test-package)

(defun test-hello ()
  (fbz:hello))

(in-package :cl21-test.package)

(ok (not (fboundp 'test-package::hello)))
(ok (find-package :test-package))
(ok (not (find-package :fbz)))
(ok (find-package :fbz :test-package))
(is (test-package:test-hello) "Hello!")

(rename-package :test-package :test-pkg)
(ok (not (find-package :fbz)))
(ok (not (find-package :fbz :test-package)))
(ok (find-package :fbz :test-pkg))
(ok (not (find-package :fbz)))
(is (test-pkg:test-hello) "Hello!")

(is-type (package-readtable *package*)
         'readtable)

(finalize)

(when (find-package :test-pkg)
  (delete-package :test-pkg))

;; use-syntax

(cl21:defpackage cl21-test.package2
  (:use :cl
        :cl-test-more))
(cl21:in-package :cl21-test.package2)

(plan 2)

(is "aiueo\n" "aiueon")

(cl21:defpackage cl21-test.package2
  (:use :cl
        :cl-test-more)
  (:use-syntax :cl21))
(cl21:in-package :cl21-test.package2)

(is "aiueo\n" "aiueo
")

(finalize)
