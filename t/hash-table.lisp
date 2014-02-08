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

(setf (getf *hash* :name) "Eitarow Fukamachi")
(setf (getf *hash* :living) "Japan")

(is (getf *hash* :name)
    "Eitarow Fukamachi")

(let ((plist (coerce *hash* 'plist)))
  (is (getf plist :living)
      "Japan")
  (is (getf plist :name)
      "Eitarow Fukamachi"))

(defclass my-hash-table (abstract-hash-table)
  ((%hash :initform (make-hash-table :test 'equal))))

(ok (hash-table-p (make-instance 'my-hash-table)))
(ok (typep (make-instance 'my-hash-table) 'hash-table))
(ok (not (typep (make-instance 'my-hash-table) 'cl-hash-table)))

(defmethod gethash (key (hash my-hash-table) &optional (default 'undefined))
  (multiple-value-bind (value existsp)
      (gethash key (slot-value hash '%hash) default)
    (if existsp
        (values (format nil "[~A]" value) existsp)
        (values value existsp))))

(defmethod (setf gethash) (val key (hash my-hash-table))
  (setf (gethash key (slot-value hash '%hash)) val))

(let ((hash (make-instance 'my-hash-table)))
  (is (gethash :dummy hash)
      'undefined)
  (setf (gethash :dummy hash) "Hi")
  (is (gethash :dummy hash)
      "[Hi]"))

(finalize)
