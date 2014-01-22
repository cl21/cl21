(in-package :cl-user)
(defpackage cl21.control
  (:use :cl)
  (:import-from :alexandria
                :if-let
                :when-let
                :xor
                :destructuring-case
                :unwind-protect-case)
  (:export :defconstant
           :defparameter
           :defvar
           :destructuring-bind
           :let
           :let*
           :progv
           :setq
           :psetq
           :block
           :catch
           :go
           :return-from
           :return
           :tagbody
           :throw
           :unwind-protect
           :not
           :eq
           :eql
           :identity
           :complement
           :constantly
           :every
           :some
           :notevery
           :notany
           :and
           :cond
           :if
           :or
           :when
           :unless
           :case
           :ccase
           :ecase
           :typecase
           :ctypecase
           :etypecase
           :otherwise
           :multiple-value-bind
           :multiple-value-call
           :multiple-value-list
           :multiple-value-prog1
           :multiple-value-setq
           :values
           :values-list
           :multiple-values-limit
           :nth-value
           :prog
           :prog*
           :prog1
           :prog2
           :progn
           :define-modify-macro
           :defsetf
           :define-setf-expander
           :get-setf-expansion
           :setf
           :psetf
           :shiftf
           :rotatef
           :control-error
           :program-error
           :undefined-function

           :if-let
           :when-let
           :xor

           :destructuring-case
           :unwind-protect-case

           :with-slots
           :with-accessors))
