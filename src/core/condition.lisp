(in-package :cl-user)
(defpackage cl21.core.condition
  (:use :cl)
  (:export :condition
           :warning
           :style-warning
           :serious-condition
           :cell-error
           :cell-error-name
           :parse-error
           :storage-condition
           :assert
           :error
           :cerror
           :warn
           :signal
           :check-type
           :invalid-method-error
           :method-combination-error
           :simple-condition
           :simple-condition-format-control
           :simple-condition-format-arguments
           :simple-error
           :simple-warning
           :invoke-debugger
           :break
           :*debugger-hook*
           :*break-on-signals*
           :handler-bind
           :handler-case
           :ignore-errors
           :define-condition
           :make-condition
           :restart
           :compute-restarts
           :find-restart
           :invoke-restart
           :invoke-restart-interactively
           :restart-bind
           :restart-case
           :restart-name
           :with-condition-restarts
           :with-simple-restart
           :abort
           :continue
           :muffle-warning
           :store-value
           :use-value

           :control-error
           :program-error

           :reader-error))
(in-package :cl21.core.condition)

(define-condition abstract-method-unimplemented (simple-error)
  ((class-name :type symbol
               :initarg :class-name)
   (method-name :type (or symbol list)
                :initarg :method-name))
  (:documentation "An error raised if the abstract-sequence class has not specified a mandatory method.")
  (:report
   (lambda (condition stream)
     (format stream
             "Method ~S for ~S must be implemented."
             (slot-value condition 'method-name)
             (slot-value condition 'class-name)))))

(defun method-unimplemented-error (method-name sequence)
  (error 'abstract-method-unimplemented
         :class-name (class-name (class-of sequence))
         :method-name method-name))
