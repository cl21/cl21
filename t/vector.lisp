(in-package :cl21-user)
(defpackage cl21-test.vector
  (:use :cl21
        :cl-test-more))
(in-package :cl21-test.vector)

(plan 11)

(defparameter *vector*
  (make-array 0 :adjustable t :fill-pointer 0))

(push 1 *vector*)
(is (elt *vector* 0) 1)

(push 3 *vector*)
(is (elt *vector* 0) 1)
(is (elt *vector* 1) 3)

(push 5 *vector*)
(is (elt *vector* 0) 1)
(is (elt *vector* 1) 3)
(is (elt *vector* 2) 5)
(is (length *vector*) 3)

(pushnew 5 *vector*)
(is (length *vector*) 3)

(is (pop *vector*) 5)
(is (pop *vector*) 3)
(is (pop *vector*) 1)

(finalize)
