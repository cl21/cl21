(in-package :cl21-user)
(defpackage cl21-test.package
  (:use :cl21
        :cl-test-more))
(in-package :cl21-test.package)

(plan 6)

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

(cl21:defpackage test-package
  (:use :cl21
        (:foo.bar.baz :as :fbz))
  (:export :test-hello))
(cl21:in-package :test-package)

(defun test-hello ()
  (fbz:hello))

(in-package :cl21-test.package)

(ok (not (fboundp 'test-package::hello)))
(ok (find-package :test-package))
(is (test-package:test-hello) "Hello!")

(finalize)
