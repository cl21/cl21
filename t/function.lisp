(in-package :cl21-user)
(defpackage cl21-test.function
  (:use :cl21
        :cl-test-more))
(in-package :cl21-test.function)

(plan 12)

(is (map (compose #'sin #'1+) '(1 2 3))
    '(0.9092974 0.14112 -0.7568025))
(is (map #'(compose sin 1+) '(1 2 3))
    '(0.9092974 0.14112 -0.7568025))
(is (keep-if (conjoin #'integerp #'evenp) '(1 2 3 2.0 4))
    '(2 4))
(is (keep-if (disjoin #'oddp #'zerop) (0.. 10))
    '(0 1 3 5 7 9))

(is (keep-if #'(and integerp evenp) '(1 2 3 2.0 4))
    '(2 4))
(is (keep-if #'(and integerp (or oddp zerop)) (0.. 10))
    '(0 1 3 5 7 9))
(is (funcall #'(not zerop) 0.0)
    nil)

(ok (map ^(random 10) (iota 10))
    "no arguments")

(is (map ^(coerce % 'list) (list #(1 2 3) "abc" #()))
    '((1 2 3) (#\a #\b #\c) NIL)
    "one argument")

(ok (every ^(eql % %1)
           (map ^(random 10) (iota 10)))
    "% is an alias for %1")

(is (sort '(2 7 6 5 2 0 6 0 9 9) ^(< %1 %2))
    '(0 0 2 2 5 6 6 7 9 9)
    "two arguments")

(is (^%2 "Hello" "World") "World"
    "ignore the first")

(finalize)
