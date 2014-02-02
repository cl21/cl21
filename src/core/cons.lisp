(in-package :cl-user)
(defpackage cl21.core.cons
  (:use :cl)
  (:import-from :alexandria
                :iota
                :remove-from-plist
                :delete-from-plist
                :ensure-list
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
           :butlast
           :nbutlast
           :ldiff
           :tailp
           :rest
           :member
           :member-if
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
           :flatten

           :list-nth
           :list-nthcdr
           :list-push
           :list-pushnew
           :list-pop

           :first
           :second
           :third
           :fourth
           :fifth
           :sixth
           :seventh
           :eighth
           :ninth
           :tenth
           :last

           ;; Alexandria
           :mappend))
(in-package :cl21.core.cons)

(setf (symbol-function 'list-nth) (symbol-function 'nth))
(defun (setf list-nth) (new-value n sequence)
  (setf (cl:nth n sequence) new-value))
(setf (symbol-function 'list-nthcdr) (symbol-function 'nthcdr))
(defun (setf list-nthcdr) (new-value n sequence)
  (setf (cl:nthcdr n sequence) new-value))
(defmacro list-push (value place)
  `(cl:push ,value ,place))
(defmacro list-pushnew (value place &rest keys)
  `(cl:pushnew ,value ,place ,@keys))
(defmacro list-pop (place)
  `(cl:pop ,place))
