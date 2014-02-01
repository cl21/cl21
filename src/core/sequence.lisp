(in-package :cl-user)
(defpackage cl21.core.sequence
  (:use :cl)
  (:shadow :nth
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
           :push
           :pushnew
           :pop)
  (:import-from :split-sequence
                :split-sequence
                :split-sequence-if)
  (:import-from :alexandria
                :length=)
  (:export :nth
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
           :split-sequence
           :split-sequence-if

           :sequence
           :fill
           :copy-seq
           :make-sequence
           :subseq
           :reduce
           :count
           :count-if
           :length ;; TODO: Write as a generic function
           :reverse
           :nreverse
           :sort
           :stable-sort

           :find
           :find-if
           :position
           :position-if

           :search
           :mismatch
           :replace
           :concatenate
           :elt
           :merge

           :map
           :map-into

           :remove
           :remove-if
           :remove-if-not
           :filter
           :remove-duplicates

           :delete
           :delete-if
           :delete-if-not
           :delete-duplicates

           :substitute
           :substitute-if
           :nsubstitute
           :nsubstitute-if

           :push
           :pushnew
           :pop

           ;; Alexandria
           :length=

           :maptree
           :take
           :partition-by))
(in-package :cl21.core.sequence)

(defun nth (n seq)
  (typecase seq
    (list (cl:nth n seq))
    (vector (elt seq n))))

(defun (setf nth) (val n seq)
  (typecase seq
    (list (setf (cl:nth n seq) val))
    (vector (setf (aref seq n) val))))

#.`(progn
     ,@(loop for (function . n) in '((first . 0)
                                     (second . 1)
                                     (third . 2)
                                     (fourth . 3)
                                     (fifth . 4)
                                     (sixth . 5)
                                     (seventh . 6)
                                     (eighth . 7)
                                     (ninth . 8)
                                     (tenth . 9))
             collect `(defgeneric ,function (seq)
                        (:method ((seq list))
                          (,(intern (string function) :cl) seq))
                        (:method ((seq sequence))
                          (elt seq ,n)))))

(defun last (seq &optional (n 1))
  (etypecase seq
    (list (cl:last seq n))
    (sequence (elt seq (- (length seq) n)))))

(setf (symbol-function 'filter) (symbol-function 'remove-if-not))

(defmacro push (value place)
  `(typecase ,place
     (vector (vector-push-extend ,value ,place))
     (T (cl:push ,value ,place))))

(defmacro pushnew (value place &rest keys)
  `(typecase ,place
     (vector (or (find ,value ,place ,@keys)
                 (vector-push-extend ,value ,place)))
     (T (cl:pushnew ,value ,place ,@keys))))

(defmacro pop (place)
  `(typecase ,place
     (vector (cl:vector-pop ,place))
     (T (cl:pop ,place))))

(defun maptree (fn tree)
  (cond
    ((atom tree)
     (funcall fn tree))
    ((null (cdr tree))
     (list (maptree fn (car tree))))
    (T
     (cons (maptree fn (car tree))
           (maptree fn (cdr tree))))))

(defun take (n sequence)
  "Take the first `n` elements from `sequence`."
  (subseq sequence 0 n))

(defun partition-by (pred sequence)
  "Given a predicate PRED, partition SEQUENCE into two sublists, the first
of which has elements that satisfy PRED, the second which do not."
  (let ((yes nil)
        (no nil))
    (map nil
         #'(lambda (x)
             (if (funcall pred x)
                 (push x yes)
                 (push x no)))
         sequence)
    (values yes no)))
