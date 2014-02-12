(in-package :cl-user)
(defpackage cl21.core.cons
  (:use :cl)
  (:shadow :member
           :member-if)
  (:import-from :cl21.core.sequence
                :abstract-list
                :drop-while)
  (:import-from :cl21.core.util
                :define-typecase-compiler-macro)
  (:import-from :alexandria
                :iota
                :remove-from-plist
                :delete-from-plist
                :ensure-list
                :ensure-cons
                :ensure-car
                :flatten
                :mappend)
  (:export :list
           :null
           :cons
           :atom
           :consp
           :atom
           :rplaca
           :rplacd
           :nth
           :nthcdr
           :car
           :cdr
           :caar
           :cadr
           :cdar
           :cddr
           :caaar
           :caadr
           :cadar
           :caddr
           :cdaar
           :cdadr
           :cddar
           :cdddr
           :caaaar
           :caaadr
           :caadar
           :caaddr
           :cadaar
           :cadadr
           :caddar
           :cadddr
           :cdaaar
           :cdaadr
           :cdadar
           :cdaddr
           :cddaar
           :cddadr
           :cdddar
           :cddddr
           :copy-tree
           :sublis
           :nsublis
           :subst
           :subst-if
           :nsubst
           :nsubst-if
           :tree-equal
           :copy-list
           :list*
           :listp
           :make-list
           :endp
           :null
           :nconc
           :append
           :revappend
           :nreconc
           :ldiff
           :tailp
           :mapc
           :mapcar
           :mapcan
           :mapl
           :maplist
           :mapcon
           :acons
           :assoc
           :assoc-if
           :copy-alist
           :pairlis
           :rassoc
           :rassoc-if
           :get-properties
           :remf
           :intersection
           :nintersection
           :adjoin
           :set-difference
           :nset-difference
           :set-exclusive-or
           :nset-exclusive-or
           :subsetp
           :union
           :nunion
           :list-length

           :iota
           :remove-from-plist
           :delete-from-plist
           :ensure-list
           :ensure-cons
           :ensure-car
           :flatten

           :list-push
           :list-pushnew
           :list-pop

           :cons-last

           ;; Alexandria
           :mappend

           :maptree
           :1..
           :0..

           ;; Abstract List
           :member
           :member-if
           :abstract-member
           :abstract-member-if))
(in-package :cl21.core.cons)

(defmacro list-push (value place)
  `(cl:push ,value ,place))
(defmacro list-pushnew (value place &rest keys)
  `(cl:pushnew ,value ,place ,@keys))
(defmacro list-pop (place)
  `(cl:pop ,place))

(setf (symbol-function 'cons-last) #'last)

(defun maptree (fn tree)
  (labels ((rec (tree)
             (etypecase tree
               (atom (funcall fn tree))
               (cons (cons (rec (car tree))
                           (if (cdr tree)
                               (rec (cdr tree))
                               nil))))))
    (if (null tree)
        nil
        (rec tree))))

(defun 1.. (n)
  (iota n :start 1))
(defun 0.. (n)
  (iota (1+ n)))


;;
;; Abstract List


;;
;; Function: member, member-if
;; Generic Function: abstract-member, abstract-member-if

(defun member (item list &rest args &key key test)
  (declare (ignore key test))
  (etypecase list
    (list (apply #'cl:member item list args))
    (abstract-list (apply #'abstract-member item list args))))
(define-typecase-compiler-macro member (&whole form item list &rest args)
  (typecase list
    (list `(cl:member ,@(cdr form)))))

(defgeneric abstract-member (item list &key key test)
  (:method (item (list abstract-list) &key (key #'identity) (test #'eql))
    (drop-while (lambda (x)
                  (not (funcall test (funcall key x) item)))
                list)))

(defun member-if (test list &rest args &key key)
  (declare (ignore key))
  (etypecase list
    (list (apply #'cl:member-if test list args))
    (abstract-list (apply #'abstract-member-if test list args))))
(define-typecase-compiler-macro member-if (&whole form test list &rest args)
  (typecase list
    (list `(cl:member-if ,@(cdr form)))))

(defgeneric abstract-member-if (test list &key key)
  (:method (test (list abstract-list) &key key)
    (drop-while (if key
                    (lambda (x)
                      (not (funcall test (funcall key x))))
                    (complement test))
                list)))
