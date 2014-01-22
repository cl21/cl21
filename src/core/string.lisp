(in-package :cl-user)
(defpackage cl21.core.string
  (:use :cl)
  (:export :string
           :base-string
           :simple-string
           :simple-base-string
           :simple-string-p
           :char
           :schar
           :string
           :string-upcase
           :string-downcase
           :string-capitalize
           :nstring-upcase
           :nstring-downcase
           :nstring-capitalize
           :string-trim
           :string-left-trim
           :string-right-trim
           :string=
           :string/=
           :string<
           :string>
           :string<=
           :string>=
           :string-equal
           :string-not-equal
           :string-lessp
           :string-greaterp
           :string-not-greaterp
           :string-not-lessp
           :stringp
           :make-string))
