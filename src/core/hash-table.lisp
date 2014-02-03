(in-package :cl-user)
(defpackage cl21.core.hash-table
  (:use :cl)
  (:import-from :alexandria
                :plist-hash-table
                :alist-hash-table
                :hash-table-keys
                :hash-table-values
                :copy-hash-table
                :hash-table-plist)
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
           :alist-hash-table))
(in-package :cl21.core.hash-table)

(defmacro eql-hash-table (&rest contents)
  (let ((hash (gensym "HASH")))
    `(let ((,hash (make-hash-table :test 'eql)))
       (setf ,@(do ((lst contents
                         (cddr lst))
                    (acc nil))
                   ((null lst) (nreverse acc))
                   (push `(gethash ,(car lst) ,hash) acc)
                   (push (cadr lst) acc)))
       ,hash)))

(defun hash-table-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  `(eql-hash-table ,@(read-delimited-list #\} stream t)))

(defun hash-table-key-exists-p (hash key)
  (nth-value 1 (gethash hash key)))

(defmethod print-object ((object hash-table) stream)
  (format stream "~<#{~;~\@{~S ~S~^ ~_~}~;}~:>"
          (hash-table-plist object)))
