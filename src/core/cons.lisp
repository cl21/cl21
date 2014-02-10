(in-package :cl-user)
(defpackage cl21.core.cons
  (:use :cl)
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
           :0..))
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
