(in-package :cl21-user)
(defpackage cl21-test.hash-table
  (:use :cl21
        :cl-test-more))
(in-package :cl21-test.hash-table)

(plan 10)

(defparameter *hash* (make-hash-table))

(is-type *hash* 'hash-table)

(is (getf *hash* :name)
    nil)

(setf (getf *hash* :name) "Eitaro Fukamachi")
(setf (getf *hash* :living) "Japan")

(is (getf *hash* :name)
    "Eitaro Fukamachi")

(let ((plist (coerce *hash* 'plist)))
  (is (getf plist :living)
      "Japan")
  (is (getf plist :name)
      "Eitaro Fukamachi"))

(defclass my-hash-table (abstract-hash-table)
  ((%hash :initform (make-hash-table :test 'equal))))

(ok (hash-table-p (make-instance 'my-hash-table)))
(ok (typep (make-instance 'my-hash-table) 'hash-table))
(ok (typep (make-instance 'my-hash-table) 'abstract-hash-table))

(defmethod abstract-gethash ((hash my-hash-table) key &optional (default 'undefined))
  (multiple-value-bind (value existsp)
      (gethash (slot-value hash '%hash) key default)
    (if existsp
        (values (format nil "[~A]" value) existsp)
        (values value existsp))))

(defmethod (setf abstract-gethash) (val (hash my-hash-table) key)
  (setf (gethash (slot-value hash '%hash) key) val))

(let ((hash (make-instance 'my-hash-table)))
  (is (gethash hash :dummy)
      'undefined)
  (setf (gethash hash :dummy) "Hi")
  (is (gethash hash :dummy)
      "[Hi]"))

(finalize)
