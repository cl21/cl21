(in-package :cl21-user)
(defpackage cl21-test.readtable
  (:use :cl
        :cl-test-more))
(defpackage cl21-test.readtable/cl21
  (:use :cl21
        :cl-test-more))
(in-package :cl21-test.readtable)

(plan nil)

(is-print (princ "Hello, World!\n")
          "Hello, World!n")

(cl21:in-package :cl21-test.readtable/cl21)

(plan nil)

(is-print (princ "Hello, World!\n")
          "Hello, World!
")

(is-print (princ "Hello, World!\n")
          "Hello, World!
")

(cl21:in-package :cl21-test.readtable)

(is-print (princ "Hello, World!\n")
          "Hello, World!n")

(finalize)
