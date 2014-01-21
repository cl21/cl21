(defpackage #:cl21)
(in-package :cl21)

(cl:dolist (package-name '(:cl21.evaluation
                           :cl21.printer
                           :cl21.reader
                           :cl21.control
                           :cl21.iteration
                           :cl21.number
                           :cl21.function
                           :cl21.sequence
                           :cl21.cons
                           :cl21.array
                           :cl21.character
                           :cl21.string
                           :cl21.symbol
                           :cl21.stream
                           :cl21.package
                           :cl21.object
                           :cl21.structure
                           :cl21.hash-table
                           :cl21.io
                           :cl21.file
                           :cl21.condition
                           :cl21.system
                           :cl21.mop
                           :cl21.types
                           :cl21.generic
                           :cl21.environment
                           :cl21.misc))
  (cl:let ((package (cl:find-package package-name)))
    (cl:unless package
      (cl:error "Package \"~A\" doesn't exist." package-name))
    (cl:do-external-symbols (symbol package)
      (cl:shadowing-import symbol)
      (cl:export symbol))))

(in-package :cl-user)
(defpackage cl21-user
  (:use :cl21))
