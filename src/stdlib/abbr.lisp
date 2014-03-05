;; -*- mode: cl21 -*-
(in-package :cl21-user)
(defpackage cl21.abbr
  (:use :cl21)
  (:export :dbind
           :mvbind
           :mvcall
           :mvlist
           :mvsetq
           :mvprog1)
  (:documentation
   "This package contains a set of several shorthand nickname macros such as
dbind for destructuring-bind, mvbind for multiple-value-bind"))
(in-package :cl21.abbr)

(defmacro dbind (lambda-list expression &body body)
  "Synonym for destructuring-bind"
  `(destructuring-bind ,lambda-list ,expression ,@body))

(defmacro mvbind (vars value-form &body body)
  "Synonym for multiple-value-bind"
  `(multiple-value-bind ,vars ,value-form ,@body))

(defmacro mvcall (function arg &rest arguments)
  "Synonym for multiple-value-call"
  `(multiple-value-call ,function ,arg ,@arguments))

(defmacro mvlist (value-form)
  "Synonym for multiple-value-list"
  `(multiple-value-list ,value-form))

(defmacro mvsetq (vars value-form)
  "Synonym for multiple-value-setq"
  `(multiple-value-setq ,vars ,value-form))

(defmacro mvprog1 (values-form &rest forms)
  "Synonym for multiple-value-prog1"
  `(multiple-value-prog1 ,values-form ,@forms))
