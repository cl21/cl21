(in-package :cl21-user)
(defpackage cl21-test.core
  (:use :cl21
        :cl-test-more))
(in-package :cl21-test.core)

(plan 25)

(is (coerce "10" 'integer) 10
    "String -> Integer")
(is (coerce "3.1e5" 'number) 3.1e5
    "String -> Number")
(is (coerce "3.1e5" 'float) 3.1e5
    "String -> Float")
(is (coerce "name" 'symbol) '|name| :test #'eq
    "String -> Symbol")
(is (coerce "name" 'keyword) :|name|
    "String -> Keyword")
(is (coerce "name" 'number) 0
    "Non-number string -> Number")

(is (coerce 10 'string) "10"
    "Integer -> String")
(is (coerce 3.1e5 'string) "310000.0"
    "Number -> String")
(is (coerce 3.14 'string) "3.14"
    "Float -> String")
(is (coerce 10 'number) 10
    "Integer -> Number")
(is (coerce 10 'float) 10.0 :test #'eql
    "Integer -> Float")

(is (coerce '|name| 'string) "name"
    "Symbol -> String")
(is (coerce '#:|name| 'string) "name"
    "Uninterned symbol -> String")
(is (coerce :|name| 'string) "name"
    "Keyword -> String")
(is (coerce '|name| 'keyword) :|name|
    "Symbol -> Keyword")
(is (coerce '|name| 'symbol) '|name| :test #'eq
    "Symbol -> Keyword")
(let ((sym '#:|name|))
  (is (coerce sym 'symbol) sym :test #'eq
      "Uninterned symbol -> Uninterned symbol"))

(is (coerce #(1 2 3) 'list) '(1 2 3)
    "Vector -> List")
(is (coerce '(1 2 3) 'vector) #(1 2 3) :test #'equalp
    "List -> Vector")
(is (coerce '(1 2 3) 'list) '(1 2 3)
    "List -> List")
(is (coerce #(1 2 3) 'vector) #(1 2 3) :test #'equalp
    "Vector -> Vector")

(is (collecting
      (doeach (i '(1 2 3 4 5))
        (collect (* i 10))))
    '(10 20 30 40 50)
    "doeach (list)")
(is-print (doeach ((i j) '((1 2) (3 4)))
            (princ "${i} - ${j}\n"))
          "1 - 2\n3 - 4\n"
          "doeach (list) with destructuring-binding")
(is (collecting
      (doeach (i #(1 2 3 4 5))
        (collect (* i 10))))
    '(10 20 30 40 50)
    "doeach (vector)")
(is-print (doeach ((i j) #((1 2) (3 4)))
            (princ "${i} - ${j}\n"))
          "1 - 2\n3 - 4\n"
          "doeach (vector) with destructuring-binding")

(finalize)
