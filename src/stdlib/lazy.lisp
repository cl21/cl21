;; -*- mode: cl21 -*-
(in-package :cl21-user)
(defpackage cl21.lazy
  (:use :cl21)
  (:export :lazy-sequence
           :cycle)
  (:documentation
   "An implementation of lazy evaluation.
Note that the basic features are implemented with funcallable-standard-class
in Meta Object Protocol, which is not so fast because most implementations
do not optimize the call related to MOP and CLOS objects.
Future improvements are awaited."))
(in-package :cl21.lazy)

(defclass memoized-lambda ()
  ((function :type function
             :initarg :function)
   (%cache :type cons))
  (:metaclass funcallable-standard-class))

(defmethod initialize-instance :after ((this memoized-lambda) &key)
  (set-funcallable-instance-function this
                                     (lambda ()
                                       (if (slot-boundp this '%cache)
                                           (slot-value this '%cache)
                                           (progn
                                             (setf (slot-value this '%cache)
                                                   (funcall (slot-value this 'function)))
                                             (slot-value this '%cache))))))

(defmacro memoized-lambda (lambda-list &body body)
  `(make-instance 'memoized-lambda
                  :function (lambda ,lambda-list ,@body)))

(defun %copy-memoized-lambda (object)
  (declare (type memoized-lambda object))
  (let ((copied
          (make-instance 'memoized-lambda
                         :function (slot-value object 'function))))
    (when (slot-boundp object '%cache)
      (setf (slot-value copied '%cache)
            (copy-list (slot-value object '%cache))))
    copied))


(defclass lazy-sequence (abstract-list)
  ((function :type memoized-lambda
             :initarg :function)))

(defmacro lazy-sequence (&body body)
  `(make-instance 'lazy-sequence :function (memoized-lambda () ,@body)))

(defmethod abstract-first ((seq lazy-sequence))
  (if (slot-boundp seq 'function)
      (car (funcall (slot-value seq 'function)))
      (make-instance 'lazy-sequence)))

(defmethod (setf abstract-first) (newval (seq lazy-sequence))
  ;; Ensure the cache exists.
  (unless (slot-boundp seq 'function)
    (setf (slot-value seq 'function)
          (memoized-lambda ()
            (cons (make-instance 'lazy-sequence)
                  (make-instance 'lazy-sequence)))))
  (funcall (slot-value seq 'function))
  (setf (car (slot-value (slot-value seq 'function) '%cache))
        newval))

(defmethod abstract-rest ((seq lazy-sequence))
  (if (slot-boundp seq 'function)
      (cdr (funcall (slot-value seq 'function)))
      (make-instance 'lazy-sequence)))

(defmethod (setf abstract-rest) (newval (seq lazy-sequence))
  ;; Ensure the cache exists.
  (funcall (slot-value seq 'function))
  (setf (cdr (funcall (slot-value seq 'function))) newval))

(defmethod make-sequence-like ((seq lazy-sequence) length &rest args &key initial-element (initial-contents nil icp))
  (declare (ignore initial-element))
  (unless icp
    (setf initial-contents (apply #'make-list length args)))
  (reduce (lambda (x y)
            (lazy-sequence (cons x y)))
          initial-contents
          :initial-value (make-instance 'lazy-sequence)
          :from-end t))

(defmethod emptyp ((seq lazy-sequence))
  (not (slot-boundp seq 'function)))

(defmethod abstract-copy-seq ((seq lazy-sequence))
  (if (slot-boundp seq 'function)
      (make-instance 'lazy-sequence
                     :function (%copy-memoized-lambda (slot-value seq 'function)))
      (make-instance 'lazy-sequence)))

(defvar *return-lazy* t)

(defmethod abstract-split-if (pred (sequence lazy-sequence) &rest args &key count end &allow-other-keys)
  (if *return-lazy*
      (if (or (and count
                   (< count 1))
              (emptyp sequence))
          nil
          (lazy-sequence
            (multiple-value-bind (res i) (let ((*return-lazy* nil))
                                           (apply #'split-if pred sequence :count 1 args))
              (if (and end (= end i))
                  res
                  `(,@res .
                          ,(let ((*return-lazy* t))
                             (when count
                               (decf (getf args :count)))
                             (setf (getf args :start) 0)
                             (when end
                               (decf (getf args :end) i))
                             (apply #'split-if pred (drop i sequence) args)))))))
      (call-next-method)))

;; FIXME: Doesn't work for infinite sequences without :count argument.
;;
;;   This example never ends.
;;
;;     (defun fib-seq ()
;;       (labels ((rec (a b)
;;                 (lazy-sequence (cons a (rec b (+ a b))))))
;;        (rec 0 1)))
;;
;;     (take 1 (remove-if #'null (split-if #'oddp (fib-seq))))
;;
(defmethod abstract-remove-if (pred (sequence lazy-sequence) &rest args &key &allow-other-keys)
  (if *return-lazy*
      (if (emptyp sequence)
          sequence
          (lazy-sequence
            (let ((*return-lazy* nil))
              (apply #'abstract-remove-if pred sequence args))))
      (call-next-method)))

(defmethod abstract-flatten ((tree lazy-sequence))
  (if *return-lazy*
      (if (emptyp tree)
          nil
          (lazy-sequence
            (append (flatten (first tree))
                    (let ((*return-lazy* nil))
                      (flatten (rest tree))))))
      (call-next-method)))


;;
;; Useful functions

(defun cycle (elements)
  (unless (listp elements)
    (setf elements (coerce elements 'list)))
  (lazy-sequence (apply #'alexandria:circular-list elements)))
