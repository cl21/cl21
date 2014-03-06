;; -*- mode: cl21 -*-
(in-package :cl21-user)
(defpackage cl21.os
  (:use :cl21)
  (:import-from :osicat
                :environment-variable
                :makunbound-environment-variable)
  (:export :*env*
           :env
           :current-directory))
(in-package :cl21.os)


;;
;; Environment

;;
;; Example:
;;   (getf *env* "HOME")
;;   => "/Users/nitro_idiot"
;;      T
;;
;;   (setf (getf *env* "APP_ENV") "production")
;;   => "production"

(defclass env (abstract-hash-table) ())

(defmethod abstract-gethash ((env env) key &optional default)
  (if-let1 (envval (environment-variable key))
    (values envval t)
    (values default nil)))

(defmethod (setf abstract-gethash) (val (env env) key)
  (setf (environment-variable key) val)
  val)

(defmethod abstract-remhash ((env env) key)
  (if (hash-exists-p env key)
      (zerop (makunbound-environment-variable key))
      nil))

(defvar *env* (make-instance 'env))

;; `env' provides shorter way to access to environment variables.
;;
;;   (env "HOME")
;    (setf (env "APP_ENV") "production")

(defun env (name)
  (getf *env* name))

(defun (setf env) (name value)
  (setf (getf *env* name) value))


;;
;; File and Directory

(setf (symbol-function 'current-directory)
      #'osicat:current-directory)
