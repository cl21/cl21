(in-package :cl-user)
(defpackage cl21.re
  (:use :cl
        :cl-ppcre)
  (:import-from :cl21.core.readtable
                :defreadtable
                :in-readtable)
  (:export :enable-cl21-re-syntax
           :disable-cl21-re-syntax))
(in-package :cl21.re)

(defun whitespacep (char)
  (member char '(#\Space #\Tab #\Newline #\Return #\Linefeed) :test #'char=))

(defun segment-reader (stream ch n)
  (if (> n 0)
      (let ((chars))
        (do ((curr (read-char stream)
                   (read-char stream)))
            ((char= ch curr))
          (push curr chars))
        (cons (coerce (nreverse chars) 'string)
              (segment-reader stream ch (- n 1))))))

(defun modifier-reader (stream)
  (let ((char (read-char stream nil)))
    (unread-char char stream)
    (unless (whitespacep char)
      (read-preserving-whitespace stream))))

(defun regex-with-modifier (regex modifier)
  (format nil "~@[(?~(~a~))~]~a" modifier regex))

(defun regexp-reader (stream sub-char numarg)
  (declare (ignore numarg))
  (let* ((segment (segment-reader stream sub-char 1))
         (modifiers (modifier-reader stream))
         (re (regex-with-modifier (car segment) modifiers)))
    `(lambda (str) (ppcre:scan-to-strings ,re str))))

(defreadtable (cl21-re-readtable :cl21.re)
  (:merge :standard)
  (:dispatch-macro-char #\# #\/ #'regexp-reader))

(defun enable-cl21-re-syntax ()
  (in-readtable cl21-re-readtable)
  (values))

(defun disable-cl21-re-syntax ()
  (in-readtable nil)
  (values))
