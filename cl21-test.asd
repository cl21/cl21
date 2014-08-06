(in-package :cl-user)
(defpackage cl21-test-asd
  (:use :cl :asdf))
(in-package :cl21-test-asd)

(defsystem cl21-test
  :license "Public Domain"
  :depends-on (:cl21
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:test-file "cl21")
                 (:test-file "cons")
                 (:test-file "hash-table")
                 (:test-file "vector")
                 (:test-file "sequence")
                 (:test-file "function")
                 (:test-file "package")
                 (:test-file "readtable")
                 (:test-file "re")
                 (:module "core"
                          :components
                          ((:test-file "all")
                           (:test-file "generic"))))))

  :defsystem-depends-on (:cl-test-more)
  :perform (test-op :after (op c)
                    (funcall (intern #. (string :run-test-system) :cl-test-more)
                             c)
                    (asdf:clear-system c)))
