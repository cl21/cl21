(in-package :cl21-user)
(defpackage cl21.lazy
  (:use :cl21)
  (:export :lazy-sequence
           :cycle))
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
                  :initarg :function
                  :initform (lambda () nil))))

(defmacro lazy-sequence (&body body)
  `(make-instance 'lazy-sequence :function (memoized-lambda () ,@body)))

(defmethod abstract-first ((seq lazy-sequence))
  (car (funcall (slot-value seq 'lazy-function))))

(defmethod abstract-rest ((seq lazy-sequence))
  (cdr (funcall (slot-value seq 'lazy-function))))

(defmethod abstract-cons (object (seq lazy-sequence))
  (lazy-sequence (cons object seq)))

(defmethod emptyp ((seq lazy-sequence))
  (null (funcall (slot-value seq 'lazy-function))))

(defvar *return-lazy* t)

(defmethod abstract-split-sequence-if (pred (sequence lazy-sequence) &rest args &key count end &allow-other-keys)
  (if *return-lazy*
      (if (or (and count
                   (< count 1))
              (emptyp sequence))
          nil
          (lazy-sequence
            (multiple-value-bind (res i) (let ((*return-lazy* nil))
                                           (apply #'split-sequence-if pred sequence :count 1 args))
              (append
               res
               (if (and end (= end i))
                   nil
                   (let ((*return-lazy* t))
                     (when count
                       (decf (getf args :count)))
                     (setf (getf args :start) 0)
                     (when end
                       (decf (getf args :end) i))
                     (apply #'split-sequence-if pred (drop i sequence) args)))))))
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
;;     (take 1 (remove-if #'null (split-sequence-if #'oddp (fib-seq))))
;;
(defmethod abstract-remove-if (pred (sequence lazy-sequence) &rest args &key &allow-other-keys)
  (if *return-lazy*
      (if (emptyp sequence)
          nil
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
