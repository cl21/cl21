(in-package :cl21-user)
(defpackage cl21-test.function
  (:use :cl21
        :cl-test-more))
(in-package :cl21-test.function)

(plan 7)

(is (mapcar (compose #'sin #'1+) '(1 2 3))
    '(0.9092974 0.14112 -0.7568025))
(is (mapcar #'(compose sin 1+) '(1 2 3))
    '(0.9092974 0.14112 -0.7568025))
(is (filter (conjoin #'integerp #'evenp) '(1 2 3 2.0 4))
    '(2 4))
(is (filter (disjoin #'oddp #'zerop) (0.. 10))
    '(0 1 3 5 7 9))

(is (filter #'(and integerp evenp) '(1 2 3 2.0 4))
    '(2 4))
(is (filter #'(and integerp (or oddp zerop)) (0.. 10))
    '(0 1 3 5 7 9))
(is (funcall #'(not zerop) 0.0)
    nil)

(finalize)
