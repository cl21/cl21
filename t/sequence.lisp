(in-package :cl21-user)
(defpackage cl21-test.sequence
  (:use :cl21
        :cl-test-more))
(in-package :cl21-test.sequence)

(plan nil)

(is (maptree (lambda (x)
               (if (< x 5)
                   -1
                   x))
             (iota 10))
    '(-1 -1 -1 -1 -1 5 6 7 8 9)
    "maptree")

(is (maptree (lambda (x)
               (if (< x 5)
                   -1
                   x))
             '(1 (3 5) 7))
    '(-1 (-1 5) 7)
    "maptree")

(is (maptree (lambda (x)
               (if (< x 5)
                   -1
                   x))
             '(1 (3 5) . 7))
    '(-1 (-1 5) . 7)
    "maptree")

(finalize)
