(in-package :cl-user)
(defpackage cl21.condition
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
           :use-value))
