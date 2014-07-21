(in-package :cl21-user)
(defpackage cl21-test.re
  (:use :cl21
        :cl21.re
        :cl-test-more))
(in-package :cl21-test.re)

(plan 14)

(multiple-value-bind (match binds) (re-match #/^Hello, (.+?)!$/ "Hello, World!")
  (is match "Hello, World!")
  (is (elt binds 0) "World")
  (is (length binds) 1))

(multiple-value-bind (match binds) (#/^(\d{4})-(\d{2})-(\d{2})$/ "2014-01-23")
  (declare (ignore match))
  (is (elt binds 0) "2014")
  (is (elt binds 1) "01")
  (is (elt binds 2) "23"))

(multiple-value-bind (match binds) (re-match "^(\\d{4})-(\\d{2})-(\\d{2})$" "2014-01-23")
  (declare (ignore match))
  (is (elt binds 0) "2014")
  (is (elt binds 1) "01")
  (is (elt binds 2) "23"))

(is (re-replace #/a/g "Eitaro Fukamachi" "α")
    "Eitαro Fukαmαchi")
(is (re-replace "a" "Eitaro Fukamachi" "α")
    "Eitαro Fukamachi")
(is (re-replace "a" "Eitaro Fukamachi" "α" :global t)
    "Eitαro Fukαmαchi")

(is (re-split #/-/ "2014-01-23")
    '("2014" "01" "23"))
(is (re-split "-" "2014-01-23")
    '("2014" "01" "23"))

(finalize)
