(in-package :cl-user)
(defpackage cl21.sequence
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
           :last)
  (:import-from :split-sequence
                :split-sequence
                :split-sequence-if)
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
           :nsubstitute-if))
(in-package :cl21.sequence)

(defgeneric nth (n sequence)
  (:method (n (seq list))
    (nth n seq))
  (:method (n (seq vector))
    (elt seq n)))

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

(defgeneric last (seq)
  (:method ((seq list))
    (cl:last seq))
  (:method ((seq sequence))
    (elt seq (1- (length seq)))))

(setf (symbol-function 'filter) (symbol-function 'remove-if-not))
