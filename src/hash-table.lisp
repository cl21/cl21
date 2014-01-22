(in-package :cl-user)
(defpackage cl21.hash-table
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
           :alist-hash-table))
(in-package :cl21.hash-table)

(defun hash-table-key-exists-p (hash key)
  (nth-value 1 (gethash hash key)))
