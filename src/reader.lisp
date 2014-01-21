(in-package :cl-user)
(defpackage cl21.reader
  (:use :cl)
  (:export :readtable
           :copy-readtable
           :make-dispatch-macro-character
           :read
           :read-preserving-whitespace
           :read-delimited-list
           :read-from-string
           :readtable-case
           :readtablep
           :set-dispatch-macro-character
           :get-dispatch-macro-character
           :set-macro-character
           :get-macro-character
           :set-syntax-from-char
           :with-standard-io-syntax
           :*read-base*
           :*read-default-float-format*
           :*read-eval*
           :*read-suppress*
           :*readtable*
           :reader-error))
