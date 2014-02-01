(in-package :cl-user)
(defpackage cl21.core.readtable
  (:use :cl)
  (:shadow :copy-readtable)
  (:import-from :cl21.core.sequence
                :maptree)
  (:shadowing-import-from :cl21.core.package
                          :*package-readtables*
                          :find-package)
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
                :in-readtable
                :find-readtable
                :merge-readtables-into
                :reader-macro-conflict)
  (:import-from :alexandria
                :ensure-list
                :if-let
                :when-let)
  (:export
   :readtable
   :make-dispatch-macro-character
   :read
   :read-preserving-whitespace
   :read-delimited-list
   :read-from-string
   :readtable-case
   :readtablep
   :set-dispatch-macro-character
   :get-dispatch-macro-character
   :set-macro-character
   :get-macro-character
   :set-syntax-from-char
   :with-standard-io-syntax
   :*read-base*
   :*read-default-float-format*
   :*read-eval*
   :*read-suppress*
   :*readtable*

   :enable-cl21-syntax
   :disable-cl21-syntax
   :defreadtable
   :in-readtable
   :_
   :_...
   :*standard-readtable*
   :use-syntax))
(in-package :cl21.core.readtable)

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-token (stream)
    (labels ((whitespacep (char)
               (member char '(#\Space #\Tab #\Newline #\Return #\Linefeed)))
             (special-char-p (char)
               (let ((macro-fn (get-macro-character char))
                     (*readtable* (find-readtable 'cl21-standard-readtable)))
                 (and macro-fn
                      (find-if (lambda (c)
                                 (eq (get-macro-character c)
                                     macro-fn))
                               '(#\) #\( #\" #\' #\` #\,)))))
             (last-char-p (char)
               (or (whitespacep char)
                   (special-char-p char))))
      (with-output-to-string (s)
        (loop for char = (peek-char nil stream nil nil)
              while (and (not (null char))
                         (last-char-p char))
              do (write-char (read-char stream) s))
        (loop for char = (peek-char nil stream nil nil)
              until (or (null char)
                        (last-char-p char))
              do (write-char (read-char stream) s)))))

  (defun read-symbol (stream char)
    (unread-char char stream)
    (let* ((token (read-token stream))
           (pos (position #\: token))
           (*readtable* (cl:copy-readtable nil)))
      (if-let (package (and pos
                            (not (zerop pos))
                            (find-package (intern (string-upcase (subseq token 0 pos))
                                                  :keyword))))
        (values (read-from-string (concatenate 'string
                                               (package-name package)
                                               (subseq token pos))))
        (values (read-from-string token)))))

  (defun sharp-colon (stream subchar numarg)
    (declare (ignore numarg))
    (let ((*readtable* (cl:copy-readtable nil))
          (token (read-token stream)))
      (values (read-from-string (concatenate 'string "#" (string subchar)
                                             token))))))

(defreadtable (cl21-standard-readtable :cl21)
  (:merge :standard)
  (:dispatch-macro-char #\# #\: #'sharp-colon)
  (:macro-char #\" #'string-reader)
  (:dispatch-macro-char #\# #\' (function |#'-reader|)))

#.`(defreadtable cl21-package-local-nickname-syntax
     ,@(map 'list
            #'(lambda (char)
                `(:macro-char ,char #'read-symbol))
            ":!$%&*+-/0123456789<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_abcdefghijklmnopqrstuvwxyz"))

(defreadtable cl21-full-readtable
  (:merge :standard)
  (:fuze cl21-package-local-nickname-syntax)
  (:fuze cl21-standard-readtable))

(defparameter *standard-readtable*
  (find-readtable 'cl21-standard-readtable))

(defun use-syntax (syntax &optional (readtable *readtable*))
  (handler-bind ((reader-macro-conflict
                   #'(lambda (c)
                       (when-let (restart (find-restart 'continue c))
                         (invoke-restart restart)))))
    (merge-readtables-into readtable syntax)))

(defun copy-readtable (&optional (from *readtable*) to)
  (if from
      (cl:copy-readtable from to)
      (cl:copy-readtable *standard-readtable* to)))

(defun enable-cl21-syntax (&optional (type :standard))
  (ecase type
    (:standard (in-readtable cl21-standard-readtable))
    (:full     (in-readtable cl21-full-readtable)))
  (values))

(defun disable-cl21-syntax ()
  (in-readtable nil)
  (values))
