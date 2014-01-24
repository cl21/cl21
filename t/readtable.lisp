(in-package :cl-user)
(defpackage cl21-test.readtable
  (:use :cl
        :cl-test-more))
(defpackage cl21-test.readtable/cl21
  (:use :cl21
        :cl-test-more))
(in-package :cl21-test.readtable)

(plan nil)

(let ((name "John"))
  (declare (ignorable name))
  (is-print (princ "Hello, ${name}!\n")
            "Hello, ${name}!n"))

(cl21:in-package :cl21-test.readtable/cl21)

(plan nil)

(is-print (princ "Hello, World!\n")
          "Hello, World!
")

(let ((name "John"))
  (declare (ignorable name))
  (is-print (princ "Hello, ${name}!\n")
            "Hello, John!
"))

(cl21:in-package :cl21-test.readtable)

(let ((name "John"))
  (declare (ignorable name))
  (is-print (princ "Hello, ${name}!\n")
            "Hello, ${name}!n"))

(finalize)
