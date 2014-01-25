(in-package :cl-user)
(defpackage cl21-test.hash-table
  (:use :cl21
        :cl-test-more))
(in-package :cl21-test.hash-table)

(plan 5)

(defparameter *hash* (make-hash-table))

(is-type *hash* 'hash-table)

(is (getf *hash* :name)
    nil)

(setf (getf *hash* :name) "Eitarow Fukamachi")
(setf (getf *hash* :living) "Japan")

(is (getf *hash* :name)
    "Eitarow Fukamachi")

(let ((plist (coerce *hash* 'plist)))
  (is (getf plist :living)
      "Japan")
  (is (getf plist :name)
      "Eitarow Fukamachi"))

(finalize)
