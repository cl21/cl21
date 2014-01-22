(in-package :cl-user)
(defpackage cl21.core.cons
  (:use :cl)
  (:import-from :alexandria
                :iota
                :remove-from-plist
                :delete-from-plist
                :ensure-list)
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
           :nthcdr
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

           :cons-last
           :list-nth
           :list-push
           :list-pushnew
           :list-pop

           :list-first
           :list-second
           :list-third
           :list-fourth
           :list-fifth
           :list-sixth
           :list-seventh
           :list-eighth
           :list-ninth
           :list-tenth))
(in-package :cl21.core.cons)

(setf (symbol-function 'cons-last) (symbol-function 'last))
(setf (symbol-function 'list-nth) (symbol-function 'nth))
(defun (setf list-nth) (new-value n sequence)
  (setf (cl:nth n sequence) new-value))
(defmacro list-push (value place)
  `(cl:push ,value ,place))
(defmacro list-pushnew (value place &rest keys)
  `(cl:pushnew ,value ,place ,@keys))
(defmacro list-pop (place)
  `(cl:pop ,place))

#.`(progn
     ,@(loop for function in '(first
                               second
                               third
                               fourth
                               fifth
                               sixth
                               seventh
                               eighth
                               ninth
                               tenth)
             append `((setf (symbol-function ',(intern (format nil "~A-~A" :list function)))
                            (symbol-function ',function))
                      (defun (setf ,(intern (format nil "~A-~A" :list function))) (new-value list)
                        (setf (cl:first list) new-value)))))
