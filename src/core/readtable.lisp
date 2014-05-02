(in-package :cl-user)
(defpackage cl21.core.readtable
  (:use :cl)
  (:shadow :copy-readtable)
  (:import-from :cl21.core.array
                :adjustable-vector)
  (:import-from :cl21.core.sequence
                :subdivide)
  (:import-from :cl21.core.cons
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
                :defreadtable
                :in-readtable
                :find-readtable)
  (:import-from :alexandria
                :if-let
                :when-let
                :ensure-list)
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
   :copy-readtable
   :*standard-readtable*
   :defsyntax
   :syntax
   :syntaxp
   :syntax-name
   :syntax-rules
   :make-syntax
   :find-syntax
   :use-syntax
   :export-syntax
   :%
   :%1
   :%2
   :%3
   :%4
   :%5))
(in-package :cl21.core.readtable)

(defun string-reader (stream char)
  (let ((*stream* stream)
        (*start-char* char)
        (*term-char* char)
        (*pair-level* 0)
        (*inner-delimiters* '())
        *saw-backslash*
        *readtable-copy*)
    (prog1
        (inner-reader nil nil nil nil)
      (read-char*))))

(defun string-interpol-reader (stream sub-char narg)
  (declare (ignore narg))
  (let ((*stream* stream)
        (*start-char* sub-char)
        (*term-char* sub-char)
        (*pair-level* 0)
        (*inner-delimiters* '((#\{ . #\})))
        *saw-backslash*
        *readtable-copy*)
    (prog1
        (inner-reader nil nil nil nil)
      (read-char*))))

(defun function-reader (stream sub-char narg)
  (declare (ignore sub-char narg))
  (let ((expr (read stream t nil t)))
    `(cl21.core.function:function ,expr)))

(defvar *in-lambda-reader* nil)

(defun lambda-reader (stream char)
  (when *in-lambda-reader*
    (error "Nested ~A()s are not allowed" char))
  (let ((form (let ((*in-lambda-reader* t))(read stream t nil t)))
        (args (make-array 5 :element-type 'bit :initial-element 0))
        (restargs (gensym))
        (%-used nil))
    (flet ((add-arg (n)
             (setf (aref args (1- n)) 1)))
      (let ((form
              (maptree (lambda (x)
                         (case x
                           (%
                            (add-arg 1)
                            (setf %-used t))
                           ((%1 %2 %3 %4 %5)
                            (add-arg
                             (parse-integer (subseq (string x) 1)))))
                         x)
                       form))
            (lambda-list '())
            (ignored-args '()))
        (dotimes (i (1+ (or (position 1 args :from-end t)
                            -1)))
          (when (zerop (aref args i))
            (push (intern (format nil "%~D" (1+ i))) ignored-args))
          (push (intern (format nil "%~D" (1+ i))) lambda-list))
        `(lambda (,@(nreverse lambda-list) &rest ,restargs ,@(and %-used '(&aux (% %1))))
           (declare (ignore ,restargs ,@(nreverse ignored-args)))
           ,form)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-token (stream)
    (labels ((whitespacep (char)
               (member char '(#\Space #\Tab #\Newline #\Return #\Linefeed)))
             (special-char-p (char)
               (let ((macro-fn (get-macro-character char))
                     (*readtable* (find-readtable :cl21)))
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
                            (funcall (intern #.(string :find-package) :cl21.core.package)
                                     (intern (string-upcase (subseq token 0 pos))
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
                                             token)))))

  (defun vector-reader (stream subchar numarg)
    (declare (ignore subchar))
    (let ((list (read-delimited-list #\) stream)))
      `(adjustable-vector
        ,@(if numarg
              `(:dimension ,numarg)
              nil)
        :initial-contents ,list)))

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
    (read-char stream)
    `(equal-hash-table ,@(read-delimited-list #\) stream t))))


;;
;; Syntax

(defstruct (syntax (:constructor %make-syntax (name &key rules))
                   (:predicate syntaxp))
  (name nil :type symbol)
  (rules nil :type (and list
                       (satisfies syntax-rule-list-p))))

(defun syntax-rule-p (rule)
  (and (listp rule)
       (listp (car rule))
       (= (length rule) 2)))

(defun syntax-rule-list-p (rules)
  (every #'syntax-rule-p rules))

(defmacro defsyntax (name &body rules)
  `(make-syntax ',name
                :rules (list ,@(mapcar (lambda (rule)
                                         `(list ',(ensure-list (car rule)) ,@(cdr rule)))
                                       rules))))

(let ((syntax-hash (make-hash-table :test 'eq))
      (exported-syntax (make-hash-table :test 'eq)))
  (defun register-syntax (name syntax)
    (setf (gethash name syntax-hash) syntax))

  (defun unregister-syntax (syntax-designator)
    (let ((name (if (syntaxp syntax-designator)
                    (syntax-name syntax-designator)
                    syntax-designator)))
      (remhash name syntax-hash)))

  (defun find-syntax (syntax-designator)
    (if (syntaxp syntax-designator)
        syntax-designator
        (values (gethash syntax-designator syntax-hash))))

  (defun make-syntax (name &key rules)
    (let ((syntax (%make-syntax name :rules rules)))
      (register-syntax name syntax)
      syntax))

  (defun export-syntax (syntax-designator &optional (package *package*))
    (let ((name (if (syntaxp syntax-designator)
                    (syntax-name syntax-designator)
                    syntax-designator)))
      (pushnew name (gethash package exported-syntax))))

  (defun set-package-export-syntaxes (package &rest syntax-names)
    (setf (gethash package exported-syntax)
          syntax-names))

  (defun exported-syntaxes (package-designator)
    (values (reverse (gethash (find-package package-designator) exported-syntax)))))

(defun use-syntax (syntax-designator &optional (readtable *readtable*))
  (let ((*readtable* readtable)
        (syntax (find-syntax syntax-designator)))
    (unless syntax
      (error "Syntax ~A doesn't exist." syntax-designator))
    (dolist (rule (syntax-rules syntax))
      (destructuring-bind ((char1 &optional char2) fn) rule
        (if char2
            (set-dispatch-macro-character char1 char2 fn)
            (set-macro-character char1 fn))))))

(let (#+ccl(*readtable* ccl::%initial-readtable%))
  (defreadtable :cl21
    (:merge #+ccl :current
            #-ccl :standard)
    (:macro-char #\" #'string-reader)
    (:dispatch-macro-char #\# #\" #'string-interpol-reader)
    (:dispatch-macro-char #\# #\' #'function-reader)
    (:macro-char #\^ #'lambda-reader)
    (:dispatch-macro-char #\# #\( #'vector-reader)
    (:dispatch-macro-char #\# #\H #'hash-table-reader)))

(defsyntax :cl21
  (#\" #'string-reader)
  ((#\# #\") #'string-interpol-reader)
  ((#\# #\') #'function-reader)
  (#\^ #'lambda-reader)
  ((#\# #\() #'vector-reader)
  ((#\# #\H) #'hash-table-reader))

#.`(defreadtable cl21-package-local-nickname-syntax
     (:merge :standard)
     ,@(map 'list
            #'(lambda (char)
                `(:macro-char ,char #'read-symbol))
            ":!$%&*+-/0123456789<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_abcdefghijklmnopqrstuvwxyz")
     (:dispatch-macro-char #\# #\: #'sharp-colon))

#.`(defsyntax cl21-package-local-nickname-syntax
     ,@(map 'list
            #'(lambda (char)
                `(,char #'read-symbol))
            ":!$%&*+-/0123456789<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_abcdefghijklmnopqrstuvwxyz")
     ((#\# #\:) #'sharp-colon))

(defreadtable cl21-full-readtable
  (:merge :standard)
  (:fuze cl21-package-local-nickname-syntax)
  (:fuze :cl21))

(defparameter *standard-readtable*
  (find-readtable :cl21))

(defun copy-readtable (&optional (from *readtable*) to)
  (if from
      (cl:copy-readtable from to)
      (cl:copy-readtable *standard-readtable* to)))

(defun enable-cl21-syntax (&optional (type :standard))
  (ecase type
    (:standard (in-readtable :cl21))
    (:full     (in-readtable cl21-full-readtable)))
  (values))

(defun disable-cl21-syntax ()
  (in-readtable nil)
  (values))
