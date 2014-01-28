(in-package :cl-user)
(defpackage cl21.core.readtable
  (:use :cl)
  (:import-from :cl21.core.sequence
                :maptree)
  (:import-from :cl-interpol
                :*stream*
                :*start-char*
                :*term-char*
                :*pair-level*
                :*inner-delimiters*
                :*saw-backslash*
                :*readtable-copy*
                :inner-reader
                :read-char*)
  (:import-from :named-readtables
                :in-readtable)
  (:import-from :alexandria
                :ensure-list)
  (:export :enable-cl21-syntax
           :disable-cl21-syntax
           :defreadtable
           :in-readtable
           :_
           :_...))
(in-package :cl21.core.readtable)

(defvar *package-readtables* (make-hash-table :test 'eq))

(defmacro defreadtable (name-and-package &rest options)
  (destructuring-bind (name &optional package) (ensure-list name-and-package)
    `(prog1
         (named-readtables:defreadtable ,name
           ,@options)
       ,@(if package
             `((setf (gethash ,package *package-readtables*)
                     ',(remove-if (lambda (option)
                                    (and (find (car option) '(:merge :fuze))
                                         (keywordp (cadr option))))
                                  options)))
             nil))))

(defun get-package-readtable-options (package-name)
  (gethash package-name *package-readtables*))

(defun string-reader (stream char)
  (let ((*stream* stream)
        (*start-char* char)
        (*term-char* char)
        (*pair-level* 0)
        (*inner-delimiters* '((#\{ . #\})))
        *saw-backslash*
        *readtable-copy*)
    (prog1
        (inner-reader nil nil nil nil)
      (read-char*))))

;; This PAPPLY macro and sharp-quote (#') reader macro are inspired by chiku's PAPPLY.
;; https://github.com/chiku-samugari/papply
(defmacro papply (&rest op-and-args)
  (let* ((lambda-list (list))
         (expr (maptree (lambda (elem)
                          (cond
                            ((eq elem '_) (let ((gensym (gensym "PARAM")))
                                            (push gensym lambda-list)
                                            gensym))
                            ((eq elem '_...) (let ((gensym (gensym "PARAM")))
                                               (push 'cl:&rest lambda-list)
                                               (push gensym lambda-list)
                                               gensym))
                            (T elem)))
                        op-and-args)))
    `(lambda ,(nreverse lambda-list)
       ,@expr)))

(defun |#'-reader| (stream sub-char narg)
  (declare (ignore sub-char narg))
  (let ((expr (read stream t nil t)))
    (if (and (consp expr)
             (symbolp (car expr))
             (not (member (car expr)
                          '(cl:lambda
                            #+sbcl sb-int:named-lambda))))
        `(papply ,expr)
        `(function ,expr))))

(defreadtable (cl21-readtable :cl21)
  (:merge :standard)
  (:macro-char #\" #'string-reader)
  (:dispatch-macro-char #\# #\' (function |#'-reader|)))

(defreadtable cl21-standard-syntax
  (:merge :standard)
  (:fuze cl21-readtable))

(defun enable-cl21-syntax ()
  (in-readtable cl21-standard-syntax)
  (values))

(defun disable-cl21-syntax ()
  (in-readtable nil)
  (values))
