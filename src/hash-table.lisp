(in-package :cl-user)
(defpackage cl21.hash-table
  (:use :cl)
  (:shadow :hash-table
           :make-hash-table
           :gethash
           :remhash
           :clrhash
           :maphash
           :hash-table-p
           :hash-table-count
           :hash-table-rehash-size
           :hash-table-rehash-threshold
           :hash-table-size
           :hash-table-test
           :with-hash-table-iterator)
  (:import-from :alexandria
                :plist-hash-table
                :alist-hash-table)
  (:export :hash-table
           :make-hash-table
           :gethash
           :remhash
           :clrhash
           :sxhash
           :maphash
           :hash-table-p
           :hash-table-count
           :hash-table-rehash-size
           :hash-table-rehash-threshold
           :hash-table-size
           :hash-table-test
           :with-hash-table-iterator

           :hash-table-keys
           :hash-table-values
           :hash-table-key-exists-p
           :copy-hash-table
           :hash-table-count
           :plist-hash-table
           :alist-hash-table))
(in-package :cl21.hash-table)

(defclass hash-table ()
  ((test :initarg :test
         :reader hash-table-test)
   (size :initarg :size
         :reader hash-table-size)
   (rehash-size :initarg :rehash-size
                :reader hash-table-rehash-size)
   (rehash-threshold :initarg :rehash-threshold
                     :reader hash-table-rehash-threshold)
   %initargs
   (%real-hash :type cl:hash-table)))

(defmethod initialize-instance ((hash hash-table) &rest initargs &key test size rehash-size rehash-threshold &allow-other-keys)
  (declare (ignore test size rehash-size rehash-threshold))
  (setf (slot-value hash '%real-hash)
        (apply #'cl:make-hash-table initargs))
  (setf (slot-value hash '%initargs)
        initargs))

(defun make-hash-table (&rest initargs &key test size rehash-size rehash-threshold &allow-other-keys)
  (declare (ignore test size rehash-size rehash-threshold))
  (apply #'make-instance 'hash-table initargs))

(defmethod print-object ((object hash-table) stream)
  (if (slot-boundp object '%real-hash)
      (princ (slot-value object '%real-hash) stream)
      (call-next-method)))

(defgeneric gethash (key hash &optional default)
  (:method (key (hash hash-table) &optional default)
    (cl:gethash key (slot-value hash '%real-hash) default)))

(defgeneric (setf gethash) (val key hash)
  (:method (val key (hash hash-table))
    (setf (cl:gethash key (slot-value hash '%real-hash))
          val)))

(defgeneric remhash (key hash)
  (:method ((hash hash-table) key)
    (cl:remhash key (slot-value hash '%real-hash))))

(defgeneric clrhash (hash)
  (:method ((hash hash-table))
    (cl:clrhash (slot-value hash '%real-hash))))

(defgeneric maphash (function hash)
  (:method (function (hash hash-table))
    (cl:maphash function (slot-value hash '%real-hash))))

(defun hash-table-p (object)
  (typep object 'hash-table))

(defgeneric hash-table-keys (hash)
  (:method ((hash hash-table))
    (alexandria:hash-table-keys (slot-value hash '%real-hash))))

(defgeneric hash-table-values (hash)
  (:method ((hash hash-table))
    (alexandria:hash-table-values (slot-value hash '%real-hash))))

(defgeneric hash-table-key-exists-p (hash key)
  (:method ((hash hash-table) key)
    (nth-value 1 (gethash hash key))))

(defgeneric copy-hash-table (hash)
  (:method ((hash hash-table))
    (with-slots (%initargs %real-hash) hash
      (let ((new-hash (apply #'allocate-instance (class-of hash)
                             %initargs)))
        (setf (slot-value new-hash '%real-hash)
              (apply #'alexandria:copy-hash-table %real-hash
                     %initargs))))))

(defgeneric hash-table-count (hash)
  (:method ((hash hash-table))
    (cl:hash-table-count (slot-value hash '%real-hash))))

(defmacro with-hash-table-iterator (mname hash-table &body body)
  `(cl:with-hash-table-iterator (,mname (typecase ,hash-table
                                          (hash-table (slot-value ,hash-table '%real-hash))
                                          (T ,hash-table)))
     ,@body))
