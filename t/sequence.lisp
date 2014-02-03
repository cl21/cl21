(in-package :cl21-user)
(defpackage cl21-test.sequence
  (:use :cl21
        :cl-test-more))
(in-package :cl21-test.sequence)

(plan nil)

(is (take 3 '(1 3 5 6 7 8))
    '(1 3 5)
    "take")
(is (take 3 #(1 3 5 6 7 8))
    #(1 3 5)
    :test #'equalp
    "take")
(is (take 3 "Hello, World!")
    "Hel"
    "take")

(is (drop 3 '(1 3 5 6 7 8))
    '(6 7 8)
    "drop")
(is (drop 3 #(1 3 5 6 7 8))
    #(6 7 8)
    :test #'equalp
    "drop")
(is (drop 3 "Hello, World!")
    "lo, World!"
    "drop")

(is (take-while #'oddp '(1 3 5 6 7 8))
    '(1 3 5)
    "take-while")
(is (take-while #'oddp '(0 1 3 5 6 7 8))
    '()
    "take-while")
(is (take-while #'oddp #(1 3 5 6 7 8))
    #(1 3 5)
    :test #'equalp
    "take-while")
(is (take-while #'oddp #(0 1 3 5 6 7 8))
    #()
    :test #'equalp
    "take-while")
(is (take-while (lambda (x) (not (char= x #\,))) "Hello, World!")
    "Hello"
    "take-while")
(is (take-while (lambda (x) (not (char= x #\,))) ",Hello, World!")
    ""
    "take-while")

(is (drop-while #'oddp '(1 3 5 6 7 8))
    '(6 7 8)
    "drop-while")
(is (drop-while #'oddp '(1 3 5 7))
    '()
    "drop-while")
(is (drop-while #'oddp #(1 3 5 6 7 8))
    #(6 7 8)
    :test #'equalp
    "drop-while")
(is (drop-while #'oddp #(1 3 5 7))
    #()
    :test #'equalp
    "drop-while")
(is (drop-while (lambda (x) (not (char= x #\,))) "Hello, World!")
    ", World!"
    "drop-while")
(is (drop-while (lambda (x) (not (char= x #\,))) "Hello World!")
    ""
    "drop-while")

(finalize)
