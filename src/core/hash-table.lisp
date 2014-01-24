(in-package :cl-user)
(defpackage cl21.core.hash-table
  (:use :cl)
  (:import-from :alexandria
                :plist-hash-table
                :alist-hash-table
                :hash-table-keys
                :hash-table-values
                :copy-hash-table)
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
           :plist-hash-table
           :alist-hash-table

           :*default-hash-table-test*))
(in-package :cl21.core.hash-table)

(defparameter *default-hash-table-test* 'eql)

(defun hash-table-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((hash (gensym "HASH")))
    `(let ((,hash (make-hash-table :test ',*default-hash-table-test*)))
       (setf ,@(do ((lst (read-delimited-list #\} stream t)
                         (cddr lst))
                    (acc nil))
                   ((null lst) (nreverse acc))
                   (push `(gethash ,(car lst) ,hash) acc)
                   (push (cadr lst) acc)))
       ,hash)))

(defun hash-table-key-exists-p (hash key)
  (nth-value 1 (gethash hash key)))
