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
               :named-readtables
               :cl-interpol
               :cl-ppcre
               :split-sequence
               :alexandria
               :cl-utilities
               :repl-utilities)
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
                   (:file "structure")
                   (:file "symbol")
                   (:file "number")
                   (:file "character")
                   (:file "cons")
                   (:file "array")
                   (:file "string")
                   (:file "string-designator")
                   (:file "sequence")
                   (:file "hash-table")
                   (:file "file")
                   (:file "stream")
                   (:file "io")
                   (:file "repl")
                   (:file "readtable")))
                 (:file "re" :depends-on ("core")))))
  :description "Common Lisp in the 21st Century."
  :in-order-to ((test-op (test-op cl21-test))))

(defmethod perform :after ((o load-op) (c (eql (find-system :cl21))))
  (declare (ignore o c))
  (pushnew :cl21 *features*))
