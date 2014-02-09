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

           :abstract-hash-table
           :abstract-gethash
           :abstract-remhash
           :abstract-clrhash
           :abstract-copy-hash-table
           :abstract-hash-table-count
           :abstract-hash-table-rehash-size
           :abstract-hash-table-rehash-threshold
           :abstract-hash-table-size
           :abstract-hash-table-test))
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
                            ``(setf ,(list* ',(intern (string (second name)) :cl) (cdddr form)) ,,(car lambda-list))
                            `(list* ',(intern (string name) :cl) (cdr form))))))))

(defun gethash (key hash &optional (default nil default-specified-p))
  (typecase hash
    (hash-table
     (apply #'cl:gethash key hash (if default-specified-p (list default) nil)))
    (otherwise
     (apply #'abstract-gethash key hash (if default-specified-p (list default) nil)))))
(defgeneric abstract-gethash (key hash &optional default))
(define-hash-compiler-macro gethash (key hash &optional default))

(defun (setf gethash) (newval key hash)
  (typecase hash
    (hash-table
     (setf (cl:gethash key hash) newval))
    (otherwise
     (setf (abstract-gethash key hash) newval))))
(defgeneric (setf abstract-gethash) (newval key hash))
(define-hash-compiler-macro (setf gethash) (newval key hash))

(defun remhash (key hash)
  (typecase hash
    (hash-table
     (cl:remhash key hash))
    (otherwise (abstract-remhash key hash))))
(defgeneric abstract-remhash (key hash))
(define-hash-compiler-macro remhash (key hash))

(defun clrhash (hash)
  (typecase hash
    (hash-table (cl:clrhash hash))
    (otherwise (abstract-clrhash hash))))
(defgeneric abstract-clrhash (hash))
(define-hash-compiler-macro clrhash (hash))

(defun maphash (function hash)
  (typecase hash
    (hash-table (cl:maphash function hash))
    (otherwise (cl:maphash function (coerce hash 'hash-table)))))
(define-hash-compiler-macro maphash (function hash))

(defun hash-table-keys (hash)
  (typecase hash
    (hash-table (alexandria:hash-table-keys hash))
    (otherwise (alexandria:hash-table-keys (coerce hash 'hash-table)))))
(define-hash-compiler-macro hash-table-keys (hash)
  `(alexandria:hash-table-keys ,hash))

(defun hash-table-values (hash)
  (typecase hash
    (hash-table (alexandria:hash-table-values hash))
    (otherwise (alexandria:hash-table-values (coerce hash 'hash-table)))))
(define-hash-compiler-macro hash-table-values (hash)
  `(alexandria:hash-table-values ,hash))

(defun copy-hash-table (hash &rest args &key key test size rehash-size rehash-threshold)
  (declare (ignore key test size rehash-size rehash-threshold))
  (typecase hash
    (hash-table (apply #'alexandria:copy-hash-table hash args))
    (otherwise (apply #'abstract-copy-hash-table hash args))))
(defgeneric abstract-copy-hash-table (hash &key key test size rehash-size rehash-threshold))
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
           (collect `(defun ,fn (hash)
                       (typecase hash
                         (hash-table
                          (,(intern (string fn) :cl) hash))
                         (otherwise (,(intern (format nil "~A-~A" :abstract fn)) hash)))))
           (collect `(defgeneric ,(intern (format nil "~A-~A" :abstract fn)) (hash)))
           (collect `(define-hash-compiler-macro ,fn (hash))))))
