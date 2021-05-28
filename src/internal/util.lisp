(in-package :cl-user)
(defpackage cl21.internal.util
  (:use :cl)
  (:import-from #+sbcl :sb-cltl2
                #+openmcl :ccl
                #+cmu :ext
                #+allegro :sys
                #+ecl :si
                #+clasp :clasp-cltl2
                #+abcl :lisp
                :variable-information)
  (:import-from :alexandria
                :ensure-list)
  (:export :define-typecase-compiler-macro))
(in-package :cl21.internal.util)

(defmacro define-typecase-compiler-macro (name lambda-list typecase)
  (flet ((remove-from-lambda-list (target lambda-list)
           (let ((pos (position target lambda-list)))
             (if pos
                 (append (if (> pos 0)
                             (subseq lambda-list 0 pos)
                             nil)
                         (subseq lambda-list (+ pos 2)))
                 lambda-list)))
         (take (target lambda-list)
           (let ((pos (position target lambda-list)))
             (if pos
                 (subseq lambda-list pos (+ pos 2))))))
    (let* ((fn-lambda-list (remove-from-lambda-list '&environment
                                                    (remove-from-lambda-list '&whole lambda-list)))
           (compiler-macro-lambda-list (append
                                        (or (take '&whole lambda-list)
                                            '(&whole form))
                                        (or (take '&environment lambda-list)
                                            '(&environment env))
                                        fn-lambda-list))
           (form-symbol (second compiler-macro-lambda-list))
           (env-symbol (nth 3 compiler-macro-lambda-list))
           (declare-ignorable `(declare (ignorable ,@(remove-if (lambda (symb) (char= (aref (string symb) 0) #\&))
                                                                fn-lambda-list)))))
      (destructuring-bind (typecase symbol &rest rules) typecase
        (declare (ignore typecase))
        `(define-compiler-macro ,name ,compiler-macro-lambda-list
           ,declare-ignorable
           (let ((type (if (constantp ,symbol)
                           (type-of ,symbol)
                           (and (symbolp ,symbol)
                                (assoc 'type
                                       (nth-value 2 (variable-information ',symbol ,env-symbol)))))))
             (if (null type)
                 ,form-symbol
                 (cond
                   ,@(mapcar (lambda (rule)
                               `((or ,@(mapcar (lambda (tp)
                                                 `(subtypep type ',tp))
                                               (ensure-list (car rule))))
                                 (destructuring-bind ,fn-lambda-list ,(if (listp name)
                                                                          `(cddr ,form-symbol)
                                                                          `(cdr ,form-symbol))
                                   ,declare-ignorable
                                   ,@(cdr rule))))
                             rules)
                   (T ,form-symbol)))))))))
