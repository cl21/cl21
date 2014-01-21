(in-package :cl-user)
(defpackage cl21.character
  (:use :cl)
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
           :char-code-limit
           :char-name
           :name-char))
