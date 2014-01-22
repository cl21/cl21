#|
  This file is a part of cl21 project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

#|
  Common Lisp in the 21st Century.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage cl21-asd
  (:use :cl :asdf))
(in-package :cl21-asd)

(defsystem cl21
  :version "0.1"
  :author "Eitarow Fukamachi"
  :license "MIT"
  :depends-on (:closer-mop
               :trivial-types
               :split-sequence
               :alexandria
               :cl-utilities
               :repl-utilities)
  :components ((:module "src"
                :serial t
                :components
                ((:file "evaluation")
                 (:file "printer")
                 (:file "reader")
                 (:file "control")
                 (:file "iteration")
                 (:file "number")
                 (:file "function")
                 (:file "sequence")
                 (:file "cons")
                 (:file "array")
                 (:file "character")
                 (:file "string")
                 (:file "symbol")
                 (:file "stream")
                 (:file "package")
                 (:file "object")
                 (:file "structure")
                 (:file "hash-table")
                 (:file "io")
                 (:file "file")
                 (:file "condition")
                 (:file "system")
                 (:file "mop")
                 (:file "types")
                 (:file "generic" :depends-on ("hash-table" "mop" "types"))
                 (:file "environment")
                 (:file "misc")
                 (:file "cl21"))))
  :description "Common Lisp in the 21st Century."
  :in-order-to ((test-op (test-op cl21-test))))
