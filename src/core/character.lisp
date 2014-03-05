(in-package :cl-user)
(defpackage cl21.core.character
  (:use :cl)
  (:import-from :alexandria
               :define-constant)
  (:export :character
           :base-char
           :standard-char
           :extended-char
           :char=
           :char/=
           :char<
           :char>
           :char<=
           :char>=
           :char-equal
           :char-not-equal
           :char-lessp
           :char-greaterp
           :char-not-greaterp
           :char-not-lessp
           :characterp
           :alpha-char-p
           :alphanumericp
           :digit-char
           :digit-char-p
           :graphic-char-p
           :standard-char-p
           :char-upcase
           :char-downcase
           :upper-case-p
           :lower-case-p
           :both-case-p
           :char-code
           :char-int
           :code-char
           :char-name
           :name-char

           ;; Variables
           :+char-code-limit+))
(in-package :cl21.core.character)

(define-constant +char-code-limit+
  char-code-limit
  :documentation #.(documentation 'char-code-limit 'variable))
