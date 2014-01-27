(in-package :cl-user)
(defpackage cl21.core
  (:use :cl)
  (:shadow :equalp
           :coerce
           :getf)
  (:import-from :alexandria
                :once-only
                :if-let
                :when-let
                :xor
                :unwind-protect-case
                :compose
                :curry
                :rcurry)
  (:import-from :cl-utilities
                :with-collectors
                :collecting
                :collect)
  (:export
   :lambda
   :compile
   :eval
   :eval-when
   :load-time-value
   :quote
   :t
   :nil

   :compiler-macro-function
   :define-compiler-macro
   :defmacro
   :macro-function
   :macroexpand
   :macroexpand-1
   :define-symbol-macro
   :symbol-macrolet
   :*macroexpand-hook*

   :proclaim
   :declaim
   :declare
   :ignore :ignorable
   :dynamic-extent
   :type
   :inline :notinline
   :ftype
   :declaration
   :optimize
   :special
   :locally
   :the
   :special-operator-p
   :constantp
   :space
   :speed
   :safety
   :debug
   :compilation-speed

   ;; Function
   :apply
   :defun
   :fdefinition
   :fboundp
   :fmakunbound
   :flet
   :labels
   :macrolet
   :funcall
   :function
   :function-lambda-expression
   :functionp
   :compiled-function-p
   :call-arguments-limit
   :lambda-list-keywords
   :lambda-parameters-limit
   :compose
   :curry
   :rcurry

   ;; Data and Control Flow
   :defconstant
   :defparameter
   :defvar
   :destructuring-bind
   :let
   :let*
   :progv
   :setq
   :psetq
   :block
   :catch
   :go
   :return-from
   :return
   :tagbody
   :throw
   :unwind-protect
   :not
   :eq
   :eql
   :equal
   :identity
   :complement
   :constantly
   :every
   :some
   :notevery
   :notany
   :and
   :cond
   :if
   :or
   :when
   :unless
   :case
   :ccase
   :ecase
   :typecase
   :ctypecase
   :etypecase
   :otherwise
   :multiple-value-bind
   :multiple-value-call
   :multiple-value-list
   :multiple-value-prog1
   :multiple-value-setq
   :values
   :values-list
   :multiple-values-limit
   :nth-value
   :prog
   :prog*
   :prog1
   :prog2
   :progn
   :define-modify-macro
   :defsetf
   :define-setf-expander
   :get-setf-expansion
   :setf
   :psetf
   :shiftf
   :rotatef
   :undefined-function
   :if-let
   :when-let
   :xor
   :unwind-protect-case
   :with-slots
   :with-accessors

   ;; Iteration
   :loop
   :do
   :do*
   :dotimes
   :dolist
   :loop-finish

   :with-collectors
   :collecting
   :collect

   :until
   :while
   :while-let
   :doeach

   ;; Printer
   :copy-pprint-dispatch
   :formatter
   :pprint-dispatch
   :pprint-exit-if-list-exhausted
   :pprint-fill
   :pprint-linear
   :pprint-tabular
   :pprint-indent
   :pprint-logical-block
   :pprint-newline
   :pprint-pop
   :pprint-tab
   :print-unreadable-object
   :set-pprint-dispatch
   :prin1
   :print
   :pprint
   :princ
   :write
   :write-to-string
   :prin1-to-string
   :princ-to-string
   :*print-array*
   :*print-base*
   :*print-radix*
   :*print-case*
   :*print-circle*
   :*print-escape*
   :*print-gensym*
   :*print-level*
   :*print-length*
   :*print-lines*
   :*print-miser-width*
   :*print-pprint-dispatch*
   :*print-pretty*
   :*print-readably*
   :*print-right-margin*
   :print-not-readable
   :print-not-readable-object
   :format

   ;; Reader
   :readtable
   :copy-readtable
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

   ;; System Construction
   :compile-file
   :compile-file-pathname
   :load
   :require  ;; Though this is deprecated, commonly used without cl: prefix.
   :with-compilation-unit
   :*features*
   :*compile-file-pathname*
   :*compile-file-truename*
   :*load-pathname*
   :*load-truename*
   :*compile-print*
   :*compile-verbose*
   :*load-print*
   :*load-verbose*

   ;; Environment
   :export :decode-universal-time
   :encode-universal-time
   :get-universal-time
   :get-decoded-time
   :sleep
   :trace
   :untrace
   :step
   :time
   :internal-time-units-per-second
   :get-internal-real-time
   :get-internal-run-time
   :disassemble
   :documentation
   :variable
   :compiler-macro
   :room
   :ed
   :dribble
   :lisp-implementation-type
   :lisp-implementation-version
   :short-site-name
   :long-site-name
   :machine-instance
   :machine-type
   :machine-version
   :software-type
   :software-version
   :user-homedir-pathname

   ;; Generic Functions
   :equalp
   :emptyp
   :getf
   :coerce

   ;; misc
   :&optional
   :&rest
   :&body
   :&environment
   :&key
   :&whole
   :&allow-other-keys
   :&aux

   :once-only))
(in-package :cl21.core)

(cl:dolist (package-name '(:cl21.core.types
                           :cl21.core.condition
                           :cl21.core.package
                           :cl21.core.object
                           :cl21.core.structure
                           :cl21.core.symbol
                           :cl21.core.number
                           :cl21.core.character
                           :cl21.core.cons
                           :cl21.core.array
                           :cl21.core.string
                           :cl21.core.string-designator
                           :cl21.core.sequence
                           :cl21.core.hash-table
                           :cl21.core.file
                           :cl21.core.stream
                           :cl21.core.io
                           :cl21.core.repl
                           :cl21.core.readtable))
  (cl:let ((package (cl:find-package package-name)))
    (cl:unless package
      (cl:error "Package \"~A\" doesn't exist." package-name))
    (cl:do-external-symbols (symbol package)
      (cl:shadowing-import symbol)
      (cl:export symbol))))

(defmacro until (expression &body body)
  "Executes `body` until `expression` is true."
  `(do ()
       (,expression)
     ,@body))

(defmacro while (expression &body body)
  "Executes `body` while `expression` is true."
  `(until (not ,expression)
     ,@body))

(defmacro while-let ((varsym expression) &body body)
  (let ((unbound '#:unbound))
    (cond
      ((listp varsym)
       `(while (destructuring-bind (&optional (,(car varsym) ',unbound) ,@(cdr varsym))
                   ,expression
                 (declare (ignore ,@(loop for sym in varsym
                                          when (string= sym :_)
                                            collect sym)))
                 (if (eq ,(car varsym) ',unbound)
                     nil
                     (progn ,@body T)))))
      (T `(let (,varsym)
            (while (setf ,varsym ,expression)
              ,@body))))))

(defmacro doeach ((varsym object) &body body)
  (labels ((replace-underscore (sym)
             (if (string= sym :_) nil sym))
           (var-list (varsym)
             (if (listp varsym)
                 (mapcar #'replace-underscore varsym)
                 (replace-underscore varsym))))
    (once-only (object)
      `(etypecase ,object
         (list (loop for ,(var-list varsym) in ,object
                     do (progn ,@body)))
         (sequence (loop for ,(var-list varsym) across ,object
                         do (progn ,@body)))
         (hash-table
          ,(if (and (listp varsym)
                    (null (cddr varsym)))
               (let ((vars (var-list varsym)))
                 `(loop for ,(car vars) being each hash-key of ,object
                        using (hash-value ,(cadr vars))
                        do (progn ,@body)))
               `(error "~A can't be destructured against the key/value pairs of a hash-table."
                       ',varsym)))))))

(defgeneric equalp (x y)
  (:method (x y)
    (cl:equalp x y)))

(defgeneric emptyp (object)
  (:method ((x null)) t)
  (:method ((x cons)) nil)
  (:method ((x vector)) (zerop (length x)))
  (:method ((x array)) (notany #'(lambda (n)
                                   (not (zerop n))) (array-dimensions x)))
  (:method ((x hash-table)) (zerop (hash-table-count x))))

(defgeneric getf (place key &optional default)
  (:method ((place hash-table) key &optional default)
    (nth-value 0 (gethash key place default)))
  (:method ((place standard-object) key &optional default)
    (if (cl:slot-boundp place key)
        (cl:slot-value place key)
        default))
  (:method ((place list) key &optional default)
    (cl:getf place key default)))

(defgeneric (setf getf) (val place key)
  (:method (val (place hash-table) key)
    (setf (gethash key place) val))
  (:method (val (place standard-object) key)
    (setf (cl:slot-value place key) val))
  (:method (val (place list) key)
    (setf (cl:getf place key) val)))

(defgeneric coerce (object output-type-spec)
  (:method ((object t) output-type-spec)
    (cl:coerce object output-type-spec)))

(defmethod coerce ((object number) output-type-spec)
  (case output-type-spec
    (string (write-to-string object))
    (T (cl:coerce object output-type-spec))))

(defmethod coerce ((object hash-table) output-type-spec)
  (ecase output-type-spec
    (plist
     (alexandria:hash-table-plist object))
    (alist
     (alexandria:hash-table-alist object))))

(defmethod coerce ((object string) output-type-spec)
  (case output-type-spec
    (integer (nth-value 0 (parse-integer object :junk-allowed t)))
    ((number float) (let ((read (read-from-string object)))
                      (if (numberp read)
                          read
                          0)))
    (symbol  (intern object))
    (keyword (intern object :keyword))
    (T (cl:coerce object output-type-spec))))

(defmethod coerce ((object symbol) output-type-spec)
  (case output-type-spec
    (string (symbol-name object))
    (keyword (intern (symbol-name object) :keyword))
    (T (cl:coerce object output-type-spec))))
