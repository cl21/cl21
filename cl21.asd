(in-package :cl-user)
(defpackage cl21-asd
  (:use :cl :asdf))
(in-package :cl21-asd)

(defsystem cl21
  :version "0.1.0"
  :license "Public Domain"
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

               ;; for cl21.os
               #-windows
               :osicat

               ;; for package-diff
               :iterate)
  :components ((:module "src"
                :components
                ((:file "cl21" :depends-on ("core"))
                 (:file "core" :depends-on ("core-subpackages"))
                 (:file "asdf" :depends-on ("cl21" "core-subpackages"))
                 (:module "core-subpackages"
                  :pathname "core"
                  :depends-on ("internal")
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
                   (:file "cons" :depends-on ("generic" "sequence" "array"))
                   (:file "array" :depends-on ("generic"))
                   (:file "string")
                   (:file "sequence" :depends-on ("generic" "condition" "environment"))
                   (:file "hash-table" :depends-on ("generic" "condition" "package" "sequence"))
                   (:file "file")
                   (:file "stream")
                   (:file "generic" :depends-on ("types"))
                   (:file "repl")
                   (:file "readtable" :depends-on ("function" "array" "sequence" "cons"))
                   (:file "environment")))
                 (:module "internal"
                  :components
                  ((:file "util")))
                 (:module "stdlib"
                  :depends-on ("core")
                  :components
                  ((:file "re")
                   #+(and (or sbcl ccl)
                          (not windows))
                   (:file "process")
                   #-windows
                   (:file "os")
                   (:file "lazy")
                   (:file "abbr")))))
               (:module "tools"
                :components
                ((:file :compare-cl-21))
                :depends-on (:src)))
  :description "Common Lisp in the 21st Century."
  :in-order-to ((test-op (test-op cl21-test))))

(defmethod perform :after ((o load-op) (c (eql (find-system :cl21))))
  (declare (ignore o c))
  (pushnew :cl21 *features*))
