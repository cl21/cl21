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
                          :emptyp)
  (:import-from :cl21.core.condition
                :method-unimplemented-error)
  (:import-from :cl21.core.sequence
                :make-sequence-iterator
                :iterator-pointer
                :iterator-next
                :iterator-endp
                :map-to
                :subdivide)
  (:import-from :cl21.core.util
                :define-typecase-compiler-macro)
  (:import-from :alexandria
                :plist-hash-table
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
  (flet ((repeated-keys-p (pairs)
           (dolist (p pairs)
             (if (< 1
                    (count (car p) pairs :key #'car))
                 (return t)))))
    (if (oddp (length contents))
        (error "Odd number of values in hash-table literal")
        (if (repeated-keys-p (subdivide contents 2))
            (error "Repeated keys in hash-table literal")
            (let ((hash (gensym "HASH")))
              `(let ((,hash (make-hash-table :test 'equal)))
                 (setf ,@(do ((lst contents
                                   (cddr lst))
                              (acc nil))
                             ((null lst) (nreverse acc))
                             (push `(gethash ,(car lst) ,hash) acc)
                             (push (cadr lst) acc)))
                 ,hash))))))

(defun hash-table-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  `(equal-hash-table ,@(read-delimited-list #\} stream t)))

(defun hash-table-key-exists-p (hash key)
  (nth-value 1 (gethash hash key)))

(defmethod print-object ((object cl:hash-table) stream)
  (format stream "~<#{~;~\@{~S ~S~^ ~_~}~;}~:>"
          (hash-table-plist object)))


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


;;
;; Function: gethash, (setf gethash)
;; Generic Function: abstract-gethash, (setf abstract-gethash)

(defun gethash (key hash &optional (default nil default-specified-p))
  (etypecase hash
    (cl:hash-table
     (apply #'cl:gethash key hash (if default-specified-p (list default) nil)))
    (abstract-hash-table
     (apply #'abstract-gethash key hash (if default-specified-p (list default) nil)))))
(define-hash-compiler-macro gethash (key hash &optional default))

(defgeneric abstract-gethash (key hash &optional default)
  (:method (key (hash abstract-hash-table) &optional default)
    (method-unimplemented-error 'abstract-gethash hash)))

(defun (setf gethash) (newval key hash)
  (etypecase hash
    (cl:hash-table
     (setf (cl:gethash key hash) newval))
    (abstract-hash-table
     (setf (abstract-gethash key hash) newval))))
(define-hash-compiler-macro (setf gethash) (newval key hash))

(defgeneric (setf abstract-gethash) (newval key hash)
  (:method (newval key (hash abstract-hash-table))
    (method-unimplemented-error '(setf abstract-gethash) hash)))


;;
;; Function: remhash
;; Generic Function: abstract-remhash

(defun remhash (key hash)
  (etypecase hash
    (cl:hash-table
     (cl:remhash key hash))
    (abstract-hash-table (abstract-remhash key hash))))
(define-hash-compiler-macro remhash (key hash))

(defgeneric abstract-remhash (key hash)
  (:method (key (hash abstract-hash-table))
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
;; Generic Function: abstract-maphash

(defun maphash (function hash)
  (etypecase hash
    (cl:hash-table (map-to 'cl:hash-table
                           (lambda (pair)
                             (funcall function (car pair) (cdr pair)))
                           hash))
    (abstract-hash-table (abstract-maphash function hash))))
(define-hash-compiler-macro maphash (function hash))

(defgeneric abstract-maphash (function hash)
  (:method (function (hash abstract-hash-table))
    (method-unimplemented-error 'abstract-maphash hash)))


;;
;; Function: hash-table-keys
;; Generic Function: abstract-hash-table-keys

(defun hash-table-keys (hash)
  (etypecase hash
    (cl:hash-table (alexandria:hash-table-keys hash))
    (abstract-hash-table (abstract-hash-table-keys hash))))
(define-hash-compiler-macro hash-table-keys (hash)
  `(alexandria:hash-table-keys ,hash))

(defgeneric abstract-hash-table-keys (hash)
  (:method ((hash abstract-hash-table))
    (let ((results '()))
      (abstract-maphash (lambda (k v)
                          (declare (ignore v))
                          (push k results))
                        hash)
      results)))


;;
;; Function: hash-table-values
;; Generic Function: abstract-hash-table-values

(defun hash-table-values (hash)
  (etypecase hash
    (cl:hash-table (alexandria:hash-table-values hash))
    (abstract-hash-table (abstract-hash-table-values hash))))
(define-hash-compiler-macro hash-table-values (hash)
  `(alexandria:hash-table-values ,hash))

(defgeneric abstract-hash-table-values (hash)
  (:method ((hash abstract-hash-table))
    (let ((results '()))
      (abstract-maphash (lambda (k v)
                          (declare (ignore k))
                          (push v results))
                        hash)
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
