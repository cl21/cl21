
(in-package :cl21-user)
(defpackage cl21-test.core.generic
  (:use :cl21
        :cl-test-more))
(in-package :cl21-test.core.generic)

(plan nil)

(let ((hash (make-hash-table :test 'eq)))
  (setf (getf hash :name) "Eitaro Fukamachi")

  (is-print (doeach ((key val) hash)
              (format t "~A: ~A\n" key val))
            "NAME: Eitaro Fukamachi\n"
            "doeach (hash-table)"))

(let ((plist nil))
  (setf (getf plist :a) 1)
  (is (getf plist :a) 1 "(setf getf) to NIL")

  ;; #63 nonexistent key
  (setf (getf plist :c) 10)
  (is (getf plist :c) 10 "(setf getf) to #63 nonexistent key"))

(let ((tree '((:a 1 :b 2) . "!")))
  (setf (getf (car tree) :b) 10)
  (is (getf (car tree) :b) 10 "(setf getf) to NIL"))

(let ((hash #h(:a 1)))
  ;; nonexistent element
  (is-print (setf (getf (getf hash :b) :c) (princ (+ 5 5))) "10" "evaluate once")
  (is (getf hash :b) '(:c 10) "(setf getf) to NIL"))

(finalize)
