(in-package :cl-user)
(defpackage cl21.core.hash-table
  (:use :cl)
  (:shadow :hash-table
           :gethash
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
                          :coerce
                          :getf
                          :emptyp)
  (:import-from :cl21.core.package
                :cl21-available-p)
  (:import-from :cl21.core.condition
                :method-unimplemented-error)
  (:import-from :cl21.core.sequence
                :make-sequence-iterator
                :iterator-pointer
                :iterator-next
                :iterator-endp)
  (:shadowing-import-from :cl21.core.sequence
                          :map)
  (:import-from :cl21.internal.util
                :define-typecase-compiler-macro)
  (:import-from :alexandria
                :alist-hash-table
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

           :hash-keys
           :hash-values
           :hash-exists-p
           :copy-hash-table
           :alist-hash-table

           :abstract-hash-table
           :abstract-gethash
           :abstract-remhash
           :abstract-clrhash
           :abstract-hash-keys
           :abstract-hash-values
           :abstract-copy-hash-table
           :abstract-hash-table-count
           :abstract-hash-table-rehash-size
           :abstract-hash-table-rehash-threshold
           :abstract-hash-table-size
           :abstract-hash-table-test))
(in-package :cl21.core.hash-table)

(defun hash-table (test-fn &rest contents)
  (let ((hash (make-hash-table :test test-fn)))
    (loop for (k v) on contents by #'cddr
          do (setf (cl:gethash k hash) v))
    hash))


(defun hash-exists-p (hash key)
  (nth-value 1 (gethash hash key)))

(defmethod print-object ((object cl:hash-table) stream)
  (if (cl21-available-p (package-name *package*))
      (format stream "~<#H(~;~@{~S ~S~^ ~_~}~;)~:>"
              (hash-table-plist object))
      (print-unreadable-object (object stream :type t :identity t)
        (format stream
                ":TEST ~S :COUNT ~S"
                (hash-table-test object)
                (hash-table-count object)))))


;;
;; Sequence Iterator

(defmethod make-sequence-iterator ((hash-table cl:hash-table) &key start end from-end)
  (assert (not (or start end from-end)))
  (with-hash-table-iterator (next hash-table)
    (make-hash-table-iterator hash-table (lambda () (next)))))

(defstruct (hash-table-iterator (:constructor %make-hash-table-iterator))
  (pointer 0 :type integer)
  (endp nil :type boolean)
  (next-kv nil :type cons)
  (next-fn nil :type function))

(defun make-hash-table-iterator (hash-table next-fn)
  (let ((emptyp (emptyp hash-table)))
    (multiple-value-bind (more key val) (funcall next-fn)
      (%make-hash-table-iterator
       :next-fn next-fn
       :next-kv (cons key val)
       :endp (or emptyp (not more))))))

(defmethod iterator-pointer ((iterator hash-table-iterator))
  (hash-table-iterator-pointer iterator))

(defmethod iterator-endp ((iterator hash-table-iterator))
  (hash-table-iterator-endp iterator))

(defmethod iterator-next ((iterator hash-table-iterator))
  (incf (hash-table-iterator-pointer iterator))
  (prog1
      (hash-table-iterator-next-kv iterator)
    (multiple-value-bind (more key val)
        (funcall (hash-table-iterator-next-fn iterator))
      (if more
          (setf (hash-table-iterator-next-kv iterator) (cons key val))
          (setf (hash-table-iterator-endp iterator) t)))))


;;
;; Macro

(defmacro define-hash-compiler-macro (name lambda-list &optional default-form)
  `(define-typecase-compiler-macro ,name (&whole form ,@lambda-list)
     (typecase hash
       (cl:hash-table ,(or default-form
                           (if (listp name)
                               ``(setf ,(list* ',(intern (string (second name)) :cl) (cdddr form)) ,,(car lambda-list))
                               `(list* ',(intern (string name) :cl) (cdr form))))))))


;;
;; Abstract Hash Table

(defclass abstract-hash-table () ())

(deftype hash-table () '(or cl:hash-table abstract-hash-table))

(defun hash-table-p (object)
  (typep object 'hash-table))

(defmethod make-sequence-iterator ((hash-table abstract-hash-table) &key start end from-end)
  (declare (ignore start end from-end))
  (method-unimplemented-error 'make-sequence-iterator hash-table))

(defmethod getf ((place abstract-hash-table) key &optional (default nil default-specified-p))
  (apply #'abstract-gethash place key (if default-specified-p
                                          (list default)
                                          nil)))

(defmethod (setf getf) (newval (place abstract-hash-table) key)
  (setf (abstract-gethash place key) newval))

(defmethod emptyp ((hash-table abstract-hash-table))
  (method-unimplemented-error 'emptyp hash-table))


;;
;; Function: gethash, (setf gethash)
;; Generic Function: abstract-gethash, (setf abstract-gethash)

(defun gethash (hash key &optional (default nil default-specified-p))
  (etypecase hash
    (cl:hash-table
     (apply #'cl:gethash key hash (if default-specified-p (list default) nil)))
    (abstract-hash-table
     (apply #'abstract-gethash hash key (if default-specified-p (list default) nil)))))
(define-hash-compiler-macro gethash (hash key &optional default))

(defgeneric abstract-gethash (hash key &optional default)
  (:method ((hash abstract-hash-table) key &optional default)
    (declare (ignore default))
    (method-unimplemented-error 'abstract-gethash hash)))

(defun (setf gethash) (newval hash key)
  (etypecase hash
    (cl:hash-table
     (setf (cl:gethash key hash) newval))
    (abstract-hash-table
     (setf (abstract-gethash hash key) newval))))
(define-hash-compiler-macro (setf gethash) (newval hash key))

(defgeneric (setf abstract-gethash) (newval hash key)
  (:method (newval key (hash abstract-hash-table))
    (method-unimplemented-error '(setf abstract-gethash) hash)))


;;
;; Function: remhash
;; Generic Function: abstract-remhash

(defun remhash (hash key)
  (etypecase hash
    (cl:hash-table
     (cl:remhash key hash))
    (abstract-hash-table (abstract-remhash hash key))))
(define-hash-compiler-macro remhash (hash key))

(defgeneric abstract-remhash (hash key)
  (:method ((hash abstract-hash-table) key)
    (declare (ignore key))
    (method-unimplemented-error 'abstract-remhash hash)))


;;
;; Function: clrhash
;; Generic Function: abstract-clrhash

(defun clrhash (hash)
  (etypecase hash
    (cl:hash-table (cl:clrhash hash))
    (abstract-hash-table (abstract-clrhash hash))))
(define-hash-compiler-macro clrhash (hash))

(defgeneric abstract-clrhash (hash)
  (:method ((hash abstract-hash-table))
    (method-unimplemented-error 'abstract-clrhash hash)))


;;
;; Function: maphash

(defun maphash (function hash)
  (map (lambda (pair)
         (funcall function (car pair) (cdr pair)))
       hash))


;;
;; Function: hash-keys
;; Generic Function: abstract-hash-keys

(defun hash-keys (hash)
  (etypecase hash
    (cl:hash-table (alexandria:hash-table-keys hash))
    (abstract-hash-table (abstract-hash-keys hash))))
(define-hash-compiler-macro hash-keys (hash)
  `(alexandria:hash-table-keys ,hash))

(defgeneric abstract-hash-keys (hash)
  (:method ((hash abstract-hash-table))
    (let ((results '())
          (iterator (make-sequence-iterator hash)))
      (do ()
          ((iterator-endp iterator))
        (push (car (iterator-next iterator))
              results))
      results)))


;;
;; Function: hash-values
;; Generic Function: abstract-hash-values

(defun hash-values (hash)
  (etypecase hash
    (cl:hash-table (alexandria:hash-table-values hash))
    (abstract-hash-table (abstract-hash-values hash))))
(define-hash-compiler-macro hash-values (hash)
  `(alexandria:hash-table-values ,hash))

(defgeneric abstract-hash-values (hash)
  (:method ((hash abstract-hash-table))
    (let ((results '())
          (iterator (make-sequence-iterator hash)))
      (do ()
          ((iterator-endp iterator))
        (push (cdr (iterator-next iterator))
              results))
      results)))


;;
;; Function: copy-hash-table
;; Generic Function: abstract-copy-hash-table

(defun copy-hash-table (hash &rest args &key key test size rehash-size rehash-threshold)
  (declare (ignore key test size rehash-size rehash-threshold))
  (etypecase hash
    (cl:hash-table (apply #'alexandria:copy-hash-table hash args))
    (abstract-hash-table (apply #'abstract-copy-hash-table hash args))))
(define-hash-compiler-macro copy-hash-table (hash &rest args &key key test size rehash-size rehash-threshold)
  `(apply #'alexandria:copy-hash-table ,hash ,args))

(defgeneric abstract-copy-hash-table (hash &key key test size rehash-size rehash-threshold)
  (:method ((hash abstract-hash-table) &key key test size rehash-size rehash-threshold)
    (declare (ignore key test size rehash-size rehash-threshold))
    (method-unimplemented-error 'abstract-copy-hash-table hash)))


;; Accessors
#.`(progn
     ,@(loop for fn in '(hash-table-count
                         hash-table-rehash-size
                         hash-table-rehash-threshold
                         hash-table-size
                         hash-table-test)
             collect `(defun ,fn (hash)
                        (etypecase hash
                          (cl:hash-table
                           (,(intern (string fn) :cl) hash))
                          (abstract-hash-table (,(intern (format nil "~A-~A" :abstract fn)) hash))))
             collect `(defgeneric ,(intern (format nil "~A-~A" :abstract fn)) (hash)
                        (:method ((hash abstract-hash-table))
                          (method-unimplemented-error ',(intern (format nil "~A-~A" :abstract fn))
                                                      hash)))
             collect `(define-hash-compiler-macro ,fn (hash))))
