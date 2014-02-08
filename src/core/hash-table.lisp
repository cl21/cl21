(in-package :cl-user)
(defpackage cl21.core.hash-table
  (:use :cl)
  (:shadow :gethash
           :remhash
           :clrhash
           :maphash
           :hash-table-p
           :hash-table-count
           :hash-table-rehash-size
           :hash-table-rehash-threshold
           :hash-table-size
           :hash-table-test)
  (:shadowing-import-from :cl21.core.generic
                          :coerce)
  (:import-from :cl21.core.util
                :define-typecase-compiler-macro)
  (:import-from :alexandria
                :plist-hash-table
                :alist-hash-table
                :hash-table-plist)
  (:import-from :cl-utilities
                :collecting
                :collect)
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

           :abstract-hash-table))
(in-package :cl21.core.hash-table)

(defmacro equal-hash-table (&rest contents)
  (let ((hash (gensym "HASH")))
    `(let ((,hash (make-hash-table :test 'equal)))
       (setf ,@(do ((lst contents
                         (cddr lst))
                    (acc nil))
                   ((null lst) (nreverse acc))
                   (push `(gethash ,(car lst) ,hash) acc)
                   (push (cadr lst) acc)))
       ,hash)))

(defun hash-table-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  `(equal-hash-table ,@(read-delimited-list #\} stream t)))

(defun hash-table-key-exists-p (hash key)
  (nth-value 1 (gethash hash key)))

(defmethod print-object ((object hash-table) stream)
  (format stream "~<#{~;~\@{~S ~S~^ ~_~}~;}~:>"
          (hash-table-plist object)))


;;
;; Abstract Hash Table

(defclass abstract-hash-table () ())

(defun hash-table-p (object)
  (or (typep object 'hash-table)
      (typep object 'abstract-hash-table)))


;;
;; Generic functions

(defmacro define-hash-compiler-macro (name lambda-list &optional default-form)
  `(define-typecase-compiler-macro ,name (&whole form ,@lambda-list)
     (typecase hash
       (hash-table ,(or default-form
                        (if (listp name)
                            ``(setf ,(list* ',(intern (string (second name)) :cl) (cddr form)) ,,(car lambda-list))
                            `(list* ',(intern (string name) :cl) (cdr form))))))))

(defgeneric gethash (key hash &optional default)
  (:method (key (hash hash-table) &optional (default nil default-specified-p))
    (apply #'cl:gethash key hash (if default-specified-p (list default) nil))))
(define-hash-compiler-macro gethash (key hash &optional default))

(defmethod (setf gethash) (newval key (hash hash-table))
  (setf (cl:gethash key hash) newval))
(define-hash-compiler-macro (setf gethash) (newval key hash))

(defgeneric remhash (key hash)
  (:method (key (hash hash-table))
    (cl:remhash key hash)))
(define-hash-compiler-macro remhash (key hash))

(defgeneric clrhash (hash)
  (:method ((hash hash-table))
    (cl:clrhash hash)))
(define-hash-compiler-macro clrhash (hash))

(defgeneric maphash (function hash)
  (:method (function (hash hash-table))
    (cl:maphash function hash))
  (:method (function (hash abstract-hash-table))
    (cl:maphash function (coerce hash 'cl-hash-table))))
(define-hash-compiler-macro maphash (function hash))

(defgeneric hash-table-keys (hash)
  (:method ((hash hash-table))
    (alexandria:hash-table-keys hash))
  (:method ((hash abstract-hash-table))
    (let ((keys '()))
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (push k keys))
               hash)
      keys)))
(define-hash-compiler-macro hash-table-keys (hash)
  `(alexandria:hash-table-keys ,hash))

(defgeneric hash-table-values (hash)
  (:method ((hash hash-table))
    (alexandria:hash-table-values hash))
  (:method ((hash abstract-hash-table))
    (let ((values '()))
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (push v values))
               hash)
      values)))
(define-hash-compiler-macro hash-table-values (hash)
  `(alexandria:hash-table-values ,hash))

(defgeneric copy-hash-table (hash &key key test size rehash-size rehash-threshold)
  (:method ((hash hash-table) &rest args &key key test size rehash-size rehash-threshold)
    (declare (ignore key test size rehash-size rehash-threshold))
    (apply #'alexandria:copy-hash-table hash args)))
(define-hash-compiler-macro copy-hash-table (hash &rest args &key key test size rehash-size rehash-threshold)
  `(apply #'alexandria:copy-hash-table ,hash ,args))

;; Accessors
#.`(progn
     ,@(collecting
         (dolist (fn '(hash-table-count
                       hash-table-rehash-size
                       hash-table-rehash-threshold
                       hash-table-size
                       hash-table-test))
           (collect `(defgeneric ,fn (hash)
                       (:method ((hash hash-table))
                         (,(intern (string fn) :cl) hash))))
           (collect `(define-hash-compiler-macro ,fn (hash))))))
