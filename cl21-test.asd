#|
  This file is a part of cl21 project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage cl21-test-asd
  (:use :cl :asdf))
(in-package :cl21-test-asd)

(defsystem cl21-test
  :author "Eitarow Fukamachi"
  :license "MIT"
  :depends-on (:cl21
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:test-file "cl21")
                 (:test-file "hash-table")
                 (:test-file "vector"))))

  :defsystem-depends-on (:cl-test-more)
  :perform (test-op :after (op c)
                    (funcall (intern #. (string :run-test-system) :cl-test-more)
                             c)
                    (asdf:clear-system c)))
