(in-package :cl-user)
(defpackage cl21-test.hash-table
  (:use :cl21
        :cl-test-more))
(in-package :cl21-test.hash-table)

(plan 4)

(defparameter *hash* (make-hash-table))

(is-type *hash* 'hash-table)

(is (getf *hash* :name)
    nil)

(setf (getf *hash* :name) "Eitarow Fukamachi")
(setf (getf *hash* :living) "Japan")

(is (getf *hash* :name)
    "Eitarow Fukamachi")

(is (coerce *hash* 'plist)
    '(:LIVING "Japan" :NAME "Eitarow Fukamachi"))

(finalize)
