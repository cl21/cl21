(in-package :cl21-user)
(defpackage cl21.lazy
  (:use :cl21)
  (:export :lazy-sequence))
(in-package :cl21.lazy)

(defmacro memoized-lambda (lambda-list &body body)
  (with-gensyms (cache undef)
    `(let ((,cache ',undef))
       (lambda ,lambda-list
         (if (eq ',undef ,cache)
             (setq ,cache (progn ,@body))
             ,cache)))))

(defclass lazy-sequence (abstract-list)
  ((lazy-function :type function
                  :initarg :function)))

(defmethod abstract-first ((seq lazy-sequence))
  (first (funcall (slot-value seq 'lazy-function))))

(defmethod abstract-rest ((seq lazy-sequence))
  (rest (funcall (slot-value seq 'lazy-function))))

(defmethod abstract-cons (object (seq lazy-sequence))
  (lazy-sequence (cons object seq)))

(defmethod emptyp ((seq lazy-sequence))
  (null (rest seq)))

(defmacro lazy-sequence (&body body)
  `(make-instance 'lazy-sequence :function (memoized-lambda () ,@body)))
