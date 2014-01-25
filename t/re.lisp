(in-package :cl21-user)
(defpackage cl21-test.re
  (:use :cl21
        :cl21.re
        :cl-test-more))
(in-package :cl21-test.re)

(plan 7)

(multiple-value-bind (match binds) (re-match #/^Hello, (.+?)!$/ "Hello, World!")
  (is match "Hello, World!")
  (is (nth 0 binds) "World")
  (is (length binds) 1))

(multiple-value-bind (match binds) (#/^(\d{4})-(\d{2})-(\d{2})$/ "2014-01-23")
  (declare (ignore match))
  (is (nth 0 binds) "2014")
  (is (nth 1 binds) "01")
  (is (nth 2 binds) "23"))

(is (re-replace #/a/g "Eitarow Fukamachi" "α")
    "Eitαrow Fukαmαchi")

(finalize)
