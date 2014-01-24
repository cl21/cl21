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
  (:import-from :named-readtables
                :defreadtable
                :in-readtable
                :reader-macro-conflict)
  (:export :enable-cl21-syntax
           :disable-cl21-syntax
           :package-readtable))
(in-package :cl21.core.readtable)

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

(defreadtable package-readtable
  (:merge :standard)
  (:macro-char #\" #'string-reader))

(defun enable-cl21-syntax ()
  (handler-bind ((reader-macro-conflict
                   (lambda (condition)
                     (let ((continue (find-restart 'continue condition)))
                       (when continue
                         (invoke-restart continue))))))
    (in-readtable package-readtable)
    (values)))

(defun disable-cl21-syntax ()
  (in-readtable nil)
  (values))
