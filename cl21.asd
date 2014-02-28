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
               :trivial-gray-streams
               :named-readtables
               :cl-interpol
               :cl-ppcre
               :split-sequence
               :alexandria
               :repl-utilities
               #+sbcl :sb-cltl2
               ;; for package-diff
               :iterate)
  :components ((:module "src"
                :components
                ((:file "cl21" :depends-on ("core"))
                 (:file "core" :depends-on ("core-subpackages"))
                 (:module "core-subpackages"
                  :pathname "core"
                  :components
                  ((:file "types")
                   (:file "condition")
                   (:file "package" :depends-on ("readtable"))
                   (:file "object")
                   (:file "function")
                   (:file "structure")
                   (:file "symbol")
                   (:file "number")
                   (:file "character")
                   (:file "cons" :depends-on ("generic" "sequence"))
                   (:file "array")
                   (:file "string")
                   (:file "sequence" :depends-on ("generic" "condition" "environment" "util"))
                   (:file "hash-table" :depends-on ("generic" "condition" "sequence" "util"))
                   (:file "file")
                   (:file "stream")
                   (:file "generic" :depends-on ("types"))
                   (:file "repl")
                   (:file "readtable" :depends-on ("function" "array" "sequence" "hash-table" "generic"))
                   (:file "environment")
                   (:file "util" :depends-on ("environment"))))
                 (:file "re" :depends-on ("core"))
                 (:file "lazy" :depends-on ("core"))
                 (:file "abbr" :depends-on ("core"))))
               (:module "tools"
                :components
                ((:file :compare-cl-21))
                :depends-on (:src)))
  :description "Common Lisp in the 21st Century."
  :in-order-to ((test-op (test-op cl21-test))))

(defmethod perform :after ((o load-op) (c (eql (find-system :cl21))))
  (declare (ignore o c))
  (pushnew :cl21 *features*))
