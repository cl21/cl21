(in-package :cl-user)
(defpackage cl21.core
  (:use :cl)
  (:shadow :function
           :destructuring-bind
           :defconstant)
  (:import-from :cl21.core.cons
                :maptree)
  (:import-from :alexandria
                :with-gensyms
                :once-only
                :with-gensyms
                :if-let
                :when-let
                :xor
                :unwind-protect-case
                :define-constant)
  (:export
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
   :decode-universal-time
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

;; concatenating all sub-packages into cl21
(cl:dolist (package-name '(:cl21.core.types
                           :cl21.core.condition
                           :cl21.core.package
                           :cl21.core.object
                           :cl21.core.function
                           :cl21.core.structure
                           :cl21.core.symbol
                           :cl21.core.number
                           :cl21.core.character
                           :cl21.core.cons
                           :cl21.core.array
                           :cl21.core.string
                           :cl21.core.sequence
                           :cl21.core.hash-table
                           :cl21.core.file
                           :cl21.core.stream
                           :cl21.core.generic
                           :cl21.core.repl
                           :cl21.core.readtable
                           :cl21.core.environment))
  (cl:let ((package (cl:find-package package-name)))
    (cl:unless package
      (cl:error "Package \"~A\" doesn't exist." package-name))
    (cl:do-external-symbols (symbol package)
      (cl:shadowing-import symbol)
      (cl:export symbol))))

(defmacro defconstant (name initial-value &key (test ''eql) documentation)
  #.(cl:concatenate 'cl:string
                    "In cl21, cl:defconstant is overridden by alexandria:define-constant."
                    (cl:documentation 'alexandria:define-constant
                                      'cl:function))
  `(alexandria:define-constant ,name ,initial-value :test ,test :documentation ,documentation))

(defmacro destructuring-bind (lambda-list expression &body body)
  "Bind the variables in LAMBDA-LIST to the corresponding values in the
tree structure resulting from the evaluation of EXPRESSION.

CL21 Feature: NIL in LAMBDA-LIST will be ignored."
  (let* (gensym-list
         (new-lambda-list (maptree (lambda (elem)
                                     (cond
                                       ((eq elem nil) (let ((gensym (gensym "NIL")))
                                                      (push gensym gensym-list)
                                                      gensym))
                                       (T elem)))
                                   lambda-list)))
    `(cl:destructuring-bind ,new-lambda-list ,expression
       (declare (ignore ,@gensym-list))
       ,@body)))

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
  "Executes `body` while `expression` is true and binds its return value to `varsym`"
  `(let (,varsym)
     (while (setf ,varsym ,expression)
       ,@body)))

(defmacro doeach ((binding object &optional return) &body body)
" `doeach' is similar to `dolist', but it can be used with any kind of sequences.
Each body is an implicit `tagbody' and the whole form has an implicit `block'.

 (doeach (x '(\"al\" \"bob\" \"joe\"))
   (when (> (length x) 2)
     (princ #\"${x}\n\"))) ; TODO: this is an older syntax for string interpolation
;
;-> bob
;   joe

Implicit destructuring-bind is available.

 (doeach ((x y) '((1 2) (2 3) (3 4)))
   (print (+ x y)))
;-> 3
;   5
;   7

For hash-table,

 (doeach ((key value) #{'a 2 'b 3})
   (when (> value 2)
     (print key)))
;
; -> B
"
(once-only (object)
      `(block nil
         (etypecase ,object
           (sequence
            (cl:map nil
                    ,(if (listp binding)
                         (with-gensyms (elem)
                           `(lambda (,elem)
                              (destructuring-bind ,binding ,elem
                                (tagbody ,@body))))
                         `(lambda (,binding)
                            (tagbody ,@body)))
                    ,object))
           (hash-table
            ,(if (and (listp binding)
                      (null (cddr binding)))
                 `(maphash (lambda ,binding
                             (tagbody ,@body))
                           ,object)
                 `(error "binding = ~A : for a hash-table, binding should be of the form (key value)"
                         ',binding))))
         ,return)))
