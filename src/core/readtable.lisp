(in-package :cl-user)
(defpackage cl21.core.readtable
  (:use :cl)
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
  (:import-from :cl21.core.hash-table
                :hash-table-reader)
  (:import-from :named-readtables
                :in-readtable)
  (:import-from :alexandria
                :ensure-list)
  (:export :enable-cl21-syntax
           :disable-cl21-syntax
           :defreadtable
           :in-readtable))
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

(defreadtable (cl21-readtable :cl21)
  (:merge :standard)
  (:macro-char #\" #'string-reader)
  (:dispatch-macro-char #\# #\{ #'hash-table-reader)
  (:syntax-from :standard #\) #\}))

(defreadtable cl21-standard-syntax
  (:merge :standard)
  (:fuze cl21-readtable))

(defun enable-cl21-syntax ()
  (in-readtable cl21-standard-syntax)
  (values))

(defun disable-cl21-syntax ()
  (in-readtable nil)
  (values))
