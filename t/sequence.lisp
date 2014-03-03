(in-package :cl21-user)
(defpackage cl21-test.sequence
  (:use :cl21
        :cl-test-more))
(in-package :cl21-test.sequence)

(plan 295)

(is (take 3 '(1 3 5 6 7 8))
    '(1 3 5)
    "take")
(is (take 3 #(1 3 5 6 7 8))
    #(1 3 5)
    :test #'equalp
    "take")
(is (take 3 "Hello, World!")
    "Hel"
    "take")

(is (drop 3 '(1 3 5 6 7 8))
    '(6 7 8)
    "drop")
(is (drop 3 #(1 3 5 6 7 8))
    #(6 7 8)
    :test #'equalp
    "drop")
(is (drop 3 "Hello, World!")
    "lo, World!"
    "drop")

(is (take-while #'oddp '(1 3 5 6 7 8))
    '(1 3 5)
    "take-while")
(is (take-while #'oddp '(0 1 3 5 6 7 8))
    '()
    "take-while")
(is (take-while #'oddp #(1 3 5 6 7 8))
    #(1 3 5)
    :test #'equalp
    "take-while")
(is (take-while #'oddp #(0 1 3 5 6 7 8))
    #()
    :test #'equalp
    "take-while")
(is (take-while (lambda (x) (not (char= x #\,))) "Hello, World!")
    "Hello"
    "take-while")
(is (take-while (lambda (x) (not (char= x #\,))) ",Hello, World!")
    ""
    "take-while")

(is (drop-while #'oddp '(1 3 5 6 7 8))
    '(6 7 8)
    "drop-while")
(is (drop-while #'oddp '(1 3 5 7))
    '()
    "drop-while")
(is (drop-while #'oddp #(1 3 5 6 7 8))
    #(6 7 8)
    :test #'equalp
    "drop-while")
(is (drop-while #'oddp #(1 3 5 7))
    #()
    :test #'equalp
    "drop-while")
(is (drop-while (lambda (x) (not (char= x #\,))) "Hello, World!")
    ", World!"
    "drop-while")
(is (drop-while (lambda (x) (not (char= x #\,))) "Hello World!")
    ""
    "drop-while")


(defclass my-list (abstract-list)
  ((head :initarg :head)
   (tail :type my-list
         :initarg :tail)))
(defun make-my-list (&rest elements)
  (reduce (lambda (x y)
            (make-instance 'my-list
                           :head x
                           :tail y))
          elements
          :initial-value (make-instance 'my-list)
          :from-end t))
(defmethod make-sequence-like ((sequence my-list) length &rest args)
  (apply #'make-my-list (apply #'make-sequence-like '() length args)))
(defmethod abstract-first ((seq my-list))
  (slot-value seq 'head))
(defmethod abstract-rest ((seq my-list))
  (if (slot-boundp seq 'tail)
      (slot-value seq 'tail)
      (make-instance 'my-list)))
(defmethod (setf abstract-first) (newval (seq my-list))
  (setf (slot-value seq 'head) newval))
(defmethod (setf abstract-rest) (newval (seq my-list))
  (setf (slot-value seq 'tail) newval))
(defmethod emptyp ((object my-list))
  (not (slot-boundp object 'head)))


(defclass my-vector (abstract-sequence)
  ((elements :type vector :initarg :elements :initform (make-array 0 :adjustable t :fill-pointer 0))))
(defun make-my-vector (&rest elements)
  (make-instance 'my-vector :elements (make-array (length elements) :adjustable t :fill-pointer t
                                                                    :initial-contents elements)))
(defmethod make-sequence-like ((sequence my-vector) length &rest args)
  (make-instance 'my-vector :elements (apply #'make-sequence-like #() length args)))
(defmethod abstract-elt ((vec my-vector) index)
  (elt (slot-value vec 'elements) index))
(defmethod (setf abstract-elt) (newval (vec my-vector) index)
  (setf (elt (slot-value vec 'elements) index) newval))
(defmethod abstract-length ((vec my-vector))
  (length (slot-value vec 'elements)))
(defmethod emptyp ((object my-vector))
  (emptyp (slot-value object 'elements)))
(defmethod adjust-sequence ((vec my-vector) length &rest args &key initial-element initial-contents)
  (declare (ignore initial-element initial-contents))
  (setf (slot-value vec 'elements)
        (apply #'adjust-sequence (slot-value vec 'elements) length args))
  vec)


(let ((seq (make-my-list 1 2 3)))
  (let ((seq-took (take 2 seq)))
    (is-type seq-took 'my-list "take")
    (is (coerce seq-took 'list) '(1 2) "take"))
  (let ((seq-rest (drop 10 seq)))
    (is-type seq-rest 'my-list "drop")
    (ok (emptyp seq-rest) "emptyp"))
  (let ((seq-sub (subseq seq 1)))
    (is-type seq-sub 'my-list "subseq")
    (is (coerce seq-sub 'list) '(2 3) "subseq"))
  (let ((seq-sub (subseq seq 1 2)))
    (is-type seq-sub 'my-list "subseq")
    (is (coerce seq-sub 'list) '(2) "subseq"))
  (is (last seq) 3 "last")

  (is (butlast '(1 2 3)) '(1 2) "butlast")
  (is (butlast '(1 2 3) 0) '(1 2 3) "butlast")
  (let ((seq-butlast (butlast seq)))
    (is-type seq-butlast 'my-list "butlast")
    (is (coerce seq-butlast 'list) '(1 2) "butlast"))
  (let ((seq-butlast (butlast seq 0)))
    (is-type seq-butlast 'my-list "butlast")
    (is (coerce seq-butlast 'list) '(1 2 3) "butlast")))

(let ((seq (make-my-list 1 2 3)))
  (let ((pushed (push 10 seq)))
    (is-type pushed 'my-list "push")
    (is (coerce pushed 'list) '(10 1 2 3) "push")
    (is (coerce seq 'list) '(10 1 2 3) "push"))
  (let ((seq (make-my-list (list 1))))
    (push 'a (elt seq 0))
    (is (coerce seq 'list) '((a 1)) "push"))
  (let ((seq (list (list) (list) (list)))
        (index 0))
    (push 'a (elt seq (incf index)))
    (is seq '(() (a) ()) "push")
    (is index 1 "push"))
  (let ((pushed-new (pushnew 0 seq)))
    (is-type pushed-new 'my-list "pushnew")
    (is (coerce pushed-new 'list) '(0 10 1 2 3) "pushnew")
    (is (coerce seq 'list) '(0 10 1 2 3) "pushnew"))
  (let ((pushed-new (pushnew 2 seq)))
    (is-type pushed-new 'my-list "pushnew")
    (is (coerce pushed-new 'list) '(0 10 1 2 3) "pushnew")
    (is (coerce seq 'list) '(0 10 1 2 3) "pushnew"))
  (let ((pushed-new (pushnew 0.0 seq :test #'=)))
    (is-type pushed-new 'my-list "pushnew")
    (is (coerce pushed-new 'list) '(0 10 1 2 3) "pushnew")
    (is (coerce seq 'list) '(0 10 1 2 3) "pushnew"))
  (let ((poped (pop seq)))
    (is poped 0 "pop")
    (is (coerce seq 'list) '(10 1 2 3) "pop")))
(let ((seq (make-my-vector 1 2 3)))
  (let ((pushed (push 10 seq)))
    (is-type pushed 'my-vector "push")
    (is (coerce pushed 'list) '(10 1 2 3) "push")
    (is (coerce seq 'list) '(10 1 2 3) "push"))
  (let ((pushed-new (pushnew 0 seq)))
    (is-type pushed-new 'my-vector "pushnew")
    (is (coerce pushed-new 'list) '(0 10 1 2 3) "pushnew")
    (is (coerce seq 'list) '(0 10 1 2 3) "pushnew"))
  (let ((pushed-new (pushnew 2 seq)))
    (is-type pushed-new 'my-vector "pushnew")
    (is (coerce pushed-new 'list) '(0 10 1 2 3) "pushnew")
    (is (coerce seq 'list) '(0 10 1 2 3) "pushnew"))
  (let ((pushed-new (pushnew 0.0 seq :test #'=)))
    (is-type pushed-new 'my-vector "pushnew")
    (is (coerce pushed-new 'list) '(0 10 1 2 3) "pushnew")
    (is (coerce seq 'list) '(0 10 1 2 3) "pushnew"))
  (let ((poped (pop seq)))
    (is poped 0 "pop")
    (is (coerce seq 'list) '(10 1 2 3) "pop"))
  (let ((seq (list (list 0) (list 1) (list 2)))
        (index 0))
    (pop (elt seq (incf index)))
    (is seq '((0) () (2)))
    (is index 1 "pop")))

(let ((seq (make-array 3
                       :initial-contents
                       (let (result)
                         (dotimes (i 3 (nreverse result))
                           (push (make-array 1
                                             :initial-element i
                                             :adjustable t
                                             :fill-pointer t)
                                 result)))))
      (index 0))
  (pushnew 'a (aref seq (incf index)))
  (is (map (rcurry #'coerce 'list) (coerce seq 'list))
      '((0) (a 1) (2)) "pushnew")
  (is index 1 "pushnew"))

(let ((seq (make-array 1 :initial-element (list))))
  (pushnew 'a (aref seq 0))
  (is (coerce seq 'list) '((a)) "pushnew"))

(let ((seq (make-my-vector 1 2 3)))
  (let ((seq-rest (drop 3 seq)))
    (ok (emptyp seq-rest) "emptyp"))
  (is (last seq) 3 "last")

  (is (coerce (butlast seq) 'vector) #(1 2) :test #'equalp "butlast")
  (is (coerce (butlast seq 0) 'vector) #(1 2 3) :test #'equalp "butlast"))

(is (take-while #'minusp '(-2 -1 0 1 2 3))
    '(-2 -1)
    "take-while")
(is (take-while #'plusp '(2 1 1 1 2 3))
    '(2 1 1 1 2 3)
    "take-while")
(let ((res (take-while #'minusp (make-my-list -2 -1 0 1 2 3))))
  (is-type res 'my-list "take-while")
  (is (coerce res 'list) '(-2 -1) "take-while"))
(let ((res (take-while #'plusp (make-my-list 2 1 1 1 2 3))))
  (is-type res 'my-list "take-while")
  (is (coerce res 'list) '(2 1 1 1 2 3) "take-while"))
(is (drop-while #'minusp '(-1 -2 -6 -7 1 2 3 4 -5 -6 0 1))
    '(1 2 3 4 -5 -6 0 1)
    "drop-while")
(is (drop-while #'minusp '(-1 -2 -6 -7 -5 -6))
    nil
    "drop-while")
(is (drop-while #'plusp '(-1 -2 -6 -7 -5 -6))
    '(-1 -2 -6 -7 -5 -6)
    "drop-while")
(let ((dropped-seq (drop-while #'minusp (make-my-list -1 -2 -6 -7 1 2 3 4 -5 -6 0 1))))
  (is-type dropped-seq 'my-list "drop-while")
  (is (coerce dropped-seq 'list)
      '(1 2 3 4 -5 -6 0 1)
      "drop-while"))
(let ((dropped-seq (drop-while #'minusp (make-my-vector -1 -2 -6 -7 1 2 3 4 -5 -6 0 1))))
  (is (coerce dropped-seq 'list)
      '(1 2 3 4 -5 -6 0 1)
      "drop-while"))

(is (position-if (lambda (x) (zerop x)) '(-1 -2 -3 0 4)) 3 "position-if")
(is (position-if (lambda (x) (zerop x)) '(0 -1 -2 -3 0 4) :start 1)
    4
    "position-if")
(is (position-if (lambda (x) (zerop x)) '(0 -1 -2 -3 0 4) :start 1 :end 3)
    nil
    "position-if")
(is (position-if (lambda (x) (zerop x)) '(0 -1 -2 -3 0 4) :start 1 :end 5)
    4
    "position-if")
(is (position-if (lambda (x) (zerop x)) (make-my-list -1 -2 -3 0 4))
    3
    "position-if")
(is (position-if (lambda (x) (zerop x)) (make-my-list 0 -1 -2 -3 0 4) :start 1)
    4
    "position-if")
(is (position-if (lambda (x) (zerop x)) (make-my-list 0 -1 -2 -3 0 4) :start 1 :end 3)
    nil
    "position-if")
(let ((seq (make-my-vector 0 -1 -2 -3 0 4)))
  (is (position-if (lambda (x) (zerop x)) seq :start 1 :end 5)
      4
      "position-if")
  (is (coerce seq 'list)
      '(0 -1 -2 -3 0 4)
      "position-if"))
(is (position-if (lambda (x) (zerop x)) (make-my-vector -1 -2 -3 0 4))
    3
    "position-if")
(is (position-if (lambda (x) (zerop x)) (make-my-vector 0 -1 -2 -3 0 4) :start 1)
    4
    "position-if")
(is (position-if (lambda (x) (zerop x)) (make-my-vector 0 -1 -2 -3 0 4) :start 1 :end 3)
    nil
    "position-if")
(let ((seq (make-my-vector 0 -1 -2 -3 0 4)))
  (is (position-if (lambda (x) (zerop x)) seq :start 1 :end 5)
      4
      "position-if")
  (is (coerce seq 'list)
      '(0 -1 -2 -3 0 4)
      "position-if"))
(is (position 0 '(1 2 3)) nil "position")
(is (position 0 '(0 1 2 3)) 0 "position")
(is (position 0 (make-my-list 1 2 3)) nil "position")
(is (position 0 (make-my-list 0 1 2 3)) 0 "position")
(is (position 0 (make-my-vector 1 2 3)) nil "position")
(is (position 0 (make-my-vector 0 1 2 3)) 0 "position")

(is (search '(0 1) '(2 4 6 0 1 3 5)) 3 "search")
(is (search '(0 1) '(2 4 6 1 3 5)) nil "search")
(is (search "dog" "it's a dog's life") 7 "search")
(is (search '(0 1) '(2 4 6 1 3 5) :key #'oddp) 2 "search")
(is (search '(0 1) (make-my-list 2 4 6 0 1 3 5)) 3 "search")
(is (search '(0 1) (make-my-list 2 4 6 1 3 5)) nil "search")
(is (search '(0 1) (make-my-list 2 4 6 1 3 5) :key #'oddp) 2 "search")
(is (search '(0 1) (make-my-vector 2 4 6 0 1 3 5)) 3 "search")
(is (search '(0 1) (make-my-vector 2 4 6 1 3 5)) nil "search")
(is (search '(0 1) (make-my-vector 2 4 6 1 3 5) :key #'oddp) 2 "search")

(is (elt '(0 1 2 3) 2) 2 "elt")
(is (elt (make-my-list 0 1 2 3) 2) 2 "elt")
(is (elt (make-my-vector 0 1 2 3) 2) 2 "elt")

(is (remove-if (lambda (x) (zerop x))
               '(0 1 2 3 0.0 4 5))
    '(1 2 3 4 5)
    "remove-if")
(let ((seq (remove-if #'zerop
                      (make-my-list 0 1 2 3 0.0 4 5))))
  (is-type seq 'my-list "remove-if")
  (is (coerce seq 'list) '(1 2 3 4 5) "remove-if"))
(let ((seq (remove-if #'zerop
                      (make-my-list 1 0 2 3 0.0 4 5)
                      :count 1)))
  (is-type seq 'my-list "remove-if")
  (is (coerce seq 'list) '(1 2 3 0.0 4 5)
      "remove-if"))
(let ((seq (remove-if #'zerop
                      (make-my-vector 0 1 2 3 0.0 4 5))))
  (is-type seq 'my-vector "remove-if")
  (is (coerce seq 'vector) #(1 2 3 4 5) :test #'equalp "remove-if"))
(let ((seq (remove-if #'zerop
                      (make-my-vector 1 0 2 3 0.0 4 5)
                      :count 1)))
  (is-type seq 'my-vector "remove-if")
  (is (coerce seq 'vector) #(1 2 3 0.0 4 5) :test #'equalp
      "remove-if"))

(is (mismatch "abcd" "ABCDE" :test #'char-equal) 4 "mismatch")
(is (mismatch '(3 2 1 1 2 3) '(1 2 3) :from-end t) 3 "mismatch")
(is (mismatch '(1 2 3 4 5 6) '(3 4 5 6 7) :start1 2 :end2 4) NIL "mismatch")
(is (mismatch '(1 2 3) '(1 2 3)) nil "mismatch")
(is (mismatch '(3 2 1 1 2 3) (make-my-list 1 2 3) :from-end t) 3 "mismatch")
(is (mismatch '(2 1 1 2 3) (make-my-list 1 2 3) :from-end t) 2 "mismatch")
(is (mismatch '(1 2 3 4 5 6) (make-my-list 3 4 5 6 7) :start1 2 :end2 4) NIL "mismatch")
(is (mismatch '(1 2 3) (make-my-list 1 2 3)) nil "mismatch")
(is (mismatch '(3 2 1 1 2 3) (make-my-vector 1 2 3) :from-end t) 3 "mismatch")
(is (mismatch '(2 1 1 2 3) (make-my-vector 1 2 3) :from-end t) 2 "mismatch")
(is (mismatch '(1 2 3 4 5 6) (make-my-vector 3 4 5 6 7) :start1 2 :end2 4) NIL "mismatch")
(is (mismatch '(1 2 3) (make-my-vector 1 2 3)) nil "mismatch")

(is (substitute-if 0 #'evenp '((1) (2) (3) (4)) :start 2 :key #'car)
    '((1) (2) (3) 0)
    "substitute-if")
(is (substitute-if 9 #'oddp '(1 2 4 1 3 4 5))
    '(9 2 4 9 9 4 9)
    "substitute-if")
(is (substitute-if 9 #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t)
    '(1 2 4 1 3 9 5)
    "substitute-if")
(let ((subst (substitute-if 0 #'evenp (make-my-list '(1) '(2) '(3) '(4)) :start 2 :key #'car)))
  (is-type subst 'my-list "substitute-if")
  (is (coerce subst 'list) '((1) (2) (3) 0) "substitute-if"))
(let ((subst (substitute-if 9 #'oddp (make-my-list 1 2 4 1 3 4 5))))
  (is-type subst 'my-list "substitute-if")
  (is (coerce subst 'list) '(9 2 4 9 9 4 9) "substitute-if"))
(let ((subst (substitute-if 9 #'evenp (make-my-list 1 2 4 1 3 4 5) :count 1 :from-end t)))
  (is-type subst 'my-list "substitute-if")
  (is (coerce subst 'list) '(1 2 4 1 3 9 5) "substitute-if"))
(let ((subst (substitute-if 0 #'evenp (make-my-vector '(1) '(2) '(3) '(4)) :start 2 :key #'car)))
  (is-type subst 'my-vector "substitute-if")
  (is (coerce subst 'vector) #('(1) '(2) '(3) 0) :test #'equalp "substitute-if"))
(let ((subst (substitute-if 9 #'oddp (make-my-vector 1 2 4 1 3 4 5))))
  (is-type subst 'my-vector "substitute-if")
  (is (coerce subst 'vector) #(9 2 4 9 9 4 9) :test #'equalp "substitute-if"))
(let ((subst (substitute-if 9 #'evenp (make-my-vector 1 2 4 1 3 4 5) :count 1 :from-end t)))
  (is-type subst 'my-vector "substitute-if")
  (is (coerce subst 'vector) #(1 2 4 1 3 9 5) :test #'equalp "substitute-if"))
(is (substitute #\. #\SPACE "0 2 4 6") "0.2.4.6"
    "substitute")
(is (substitute 9 4 '(1 2 4 1 3 4 5)) '(1 2 9 1 3 9 5)
    "substitute")
(is (substitute 9 4 '(1 2 4 1 3 4 5) :count 1) '(1 2 9 1 3 4 5)
    "substitute")
(is (substitute 9 4 '(1 2 4 1 3 4 5) :count 1 :from-end t)
    '(1 2 4 1 3 9 5)
    "substitute")
(is (substitute 9 3 '(1 2 4 1 3 4 5) :test #'>) '(9 9 4 9 3 4 5)
    "substitute")
(is (coerce (substitute #\. #\SPACE (apply #'make-my-list (coerce "0 2 4 6" 'list))) 'string)
    "0.2.4.6"
    "substitute")
(let ((subst (substitute 9 4 (make-my-list 1 2 4 1 3 4 5))))
  (is-type subst 'my-list "substitute")
  (is (coerce subst 'list) '(1 2 9 1 3 9 5) "substitute"))
(let ((subst (substitute 9 4 (make-my-list 1 2 4 1 3 4 5) :count 1)))
  (is-type subst 'my-list "substitute")
  (is (coerce subst 'list) '(1 2 9 1 3 4 5) "substitute"))
(let ((subst (substitute 9 4 (make-my-list 1 2 4 1 3 4 5) :count 1 :from-end t)))
  (is-type subst 'my-list "substitute")
  (is (coerce subst 'list) '(1 2 4 1 3 9 5) "substitute"))
(let ((subst (substitute 9 3 (make-my-list 1 2 4 1 3 4 5) :test #'>)))
  (is-type subst 'my-list "substitute")
  (is (coerce subst 'list) '(9 9 4 9 3 4 5) "substitute"))

(is (member 2 '(1 2 3)) '(2 3)
    "member")
(is (member 'e '(a b c d)) NIL
    "member")
(is (member-if #'listp '(a b nil c d)) '(NIL C D)
    "member-if")
(is (member-if #'numberp '(a #\Space 5/3 foo)) '(5/3 FOO)
    "member-if")
(is (coerce (member 2 (make-my-list 1 2 3)) 'list) '(2 3)
    "member")
(is (coerce (member 'e (make-my-list 'a 'b 'c 'd)) 'list) NIL
    "member")
(is (coerce (member-if #'listp (make-my-list 'a 'b nil 'c 'd)) 'list) '(NIL C D)
    "member-if")
(is (coerce (member-if #'numberp (make-my-list 'a #\Space 5/3 'foo)) 'list) '(5/3 FOO)
    "member-if")

(is (split 1 '(0 1 2 3 1 2 3)) '((0) (2 3) (2 3))
    "split")
(let ((seq (split 1 (make-my-list 0 1 2 3 1 2 3))))
  (ok (every (lambda (x) (typep x 'my-list)) seq) "split")
  (is (map #'(lambda (x) (coerce x 'list))
           seq)
      '((0) (2 3) (2 3)) "split"))
(is (multiple-value-list (split nil '(1 nil 2 nil 3) :start 2)) '(((2) (3)) 5)
    "split")
(multiple-value-bind (seq i) (split nil (make-my-list 1 nil 2 nil 3) :start 2)
  (ok (every (lambda (x) (typep x 'my-list)) seq) "split")
  (is (map #'(lambda (x) (coerce x 'list)) seq)
      '((2) (3))
      "split")
  (is i 5 "split"))
(is (multiple-value-list (split nil '(1 nil 2 nil 3) :start 2 :count 1)) '(((2)) 4)
    "split")
(multiple-value-bind (seq i) (split nil (make-my-list 1 nil 2 nil 3) :start 2 :count 1)
  (ok (every (lambda (x) (typep x 'my-list)) seq) "split")
  (is (map #'(lambda (x) (coerce x 'list)) seq)
      '((2))
      "split")
  (is i 4 "split"))
(multiple-value-bind (seq i) (split 1 (make-my-list 0 1 2 3 1 2 3) :count 1)
  (ok (every (lambda (x) (typep x 'my-list)) seq) "split")
  (is (map #'(lambda (x) (coerce x 'list)) seq)
      '((0))
      "split")
  (is i 2 "split"))
(multiple-value-bind (seq i) (split 1 (make-my-list 0 1 2 3 1 1 2 3) :count 5)
  (ok (every (lambda (x) (typep x 'my-list)) seq) "split")
  (is (map #'(lambda (x) (coerce x 'list)) seq)
      '((0) (2 3) NIL (2 3))
      "split")
  (is i 8 "split"))
(multiple-value-bind (seq i) (split 1 (make-my-list 0 1 2 3 1 1 2 3)
                                    :count 5
                                    :remove-empty-subseqs t)
  (ok (every (lambda (x) (typep x 'my-list)) seq) "split")
  (is (map #'(lambda (x) (coerce x 'list)) seq)
      '((0) (2 3) (2 3))
      "split")
  (is i 8 "split"))
(multiple-value-bind (seq i) (split-if #'oddp (make-my-list 0 1 1 2 3 5 8 13 21)
                                       :count 3
                                       :remove-empty-subseqs t)
  (ok (every (lambda (x) (typep x 'my-list)) seq) "split")
  (is (map #'(lambda (x) (coerce x 'list)) seq)
      '((0) (2) (8))
      "split")
  ;; NOTE: In split-sequence library, this returns 9 as the second value.
  ;;   But it looks a bug related to :remove-empty-subseqs and :count. No need to traverse 9 elements.
  (is i 8 "split"))

(let ((seq (delete-duplicates (make-my-vector 1 2 3 11 2 10 1))))
  (is-type seq 'my-vector "delete-duplicates")
  (is (coerce seq 'list) '(3 11 2 10 1) "delete-duplicates"))
(let ((seq (delete-duplicates (make-my-vector 1 2 3 11 2 10 1) :start 1)))
  (is-type seq 'my-vector "delete-duplicates")
  (is (coerce seq 'list) '(1 3 11 2 10 1) "delete-duplicates"))
(let ((seq (delete-duplicates (make-my-vector 1 2 3 11 2 10 1) :end 5)))
  (is-type seq 'my-vector "delete-duplicates")
  (is (coerce seq 'list) '(1 3 11 2 10 1) "delete-duplicates"))
(let ((seq (delete-duplicates (make-my-vector 1 2 3 11 2 10 1) :end 5 :from-end t)))
  (is-type seq 'my-vector "delete-duplicates")
  (is (coerce seq 'list) '(1 2 3 11 10 1) "delete-duplicates"))
(let ((seq (delete-duplicates (make-my-vector 1 2 3 11 2 10 1) :from-end t)))
  (is-type seq 'my-vector "delete-duplicates")
  (is (coerce seq 'list) '(1 2 3 11 10) "delete-duplicates"))
(let ((seq (delete-duplicates (make-my-list 1 2 3 11 2 10 1))))
  (is-type seq 'my-list "delete-duplicates")
  (is (coerce seq 'list) '(3 11 2 10 1) "delete-duplicates"))
(let ((seq (delete-duplicates (make-my-list 1 2 3 11 2 10 1) :start 1)))
  (is-type seq 'my-list "delete-duplicates")
  (is (coerce seq 'list) '(1 3 11 2 10 1) "delete-duplicates"))
(let ((seq (delete-duplicates (make-my-list 1 2 3 11 2 10 1) :end 5)))
  (is-type seq 'my-list "delete-duplicates")
  (is (coerce seq 'list) '(1 3 11 2 10 1) "delete-duplicates"))
(let ((seq (delete-duplicates (make-my-list 1 2 3 11 2 10 1) :end 5 :from-end t)))
  (is-type seq 'my-list "delete-duplicates")
  (is (coerce seq 'list) '(1 2 3 11 10 1) "delete-duplicates"))
(let ((seq (delete-duplicates (make-my-list 1 2 3 11 2 10 1) :from-end t)))
  (is-type seq 'my-list "delete-duplicates")
  (is (coerce seq 'list) '(1 2 3 11 10) "delete-duplicates"))

(is (reduce #'* '(1 2 3 4 5)) 120
    "reduce")
(is (reduce #'append '((1) (2)) :initial-value '(i n i t))
    '(I N I T 1 2)
    "reduce")
(is (reduce #'append '((1) (2)) :from-end t
                                :initial-value '(i n i t))
    '(1 2 I N I T)
    "reduce")
(is (reduce #'- '(1 2 3 4)) -8
    "reduce")
(is (reduce #'+ '()) 0 "reduce")
(is (reduce #'+ '(3)) 3 "reduce")
(is (reduce #'+ '(foo)) 'foo "reduce")
(is (reduce #'list '(1 2 3 4)) '(((1 2) 3) 4) "reduce")
(is (reduce #'list '(1 2 3 4) :from-end t) '(1 (2 (3 4))) "reduce")
(is (reduce #'list '(1 2 3 4) :initial-value 'foo) '((((foo 1) 2) 3) 4) "reduce")
(is (reduce #'list '(1 2 3 4)
            :from-end t :initial-value 'foo)
    '(1 (2 (3 (4 foo))))
    "reduce")
(is (reduce #'* (make-my-list 1 2 3 4 5)) 120
    "reduce")
(is (reduce #'append (make-my-list '(1) '(2)) :initial-value '(i n i t))
    '(I N I T 1 2)
    "reduce")
(is (reduce #'append (make-my-list '(1) '(2)) :from-end t
                                              :initial-value '(i n i t))
    '(1 2 I N I T)
    "reduce")
(is (reduce #'- (make-my-list 1 2 3 4)) -8
    "reduce")
(is (reduce #'+ (make-instance 'my-list)) 0 "reduce")
(is (reduce #'+ (make-instance 'my-list) :initial-value 10) 10 "reduce")
(is (reduce #'+ (make-my-list 3)) 3 "reduce")
(is (reduce #'+ (make-my-list 'foo)) 'foo "reduce")
(is (reduce #'list (make-my-list 1 2 3 4)) '(((1 2) 3) 4) "reduce")
(is (reduce #'list (make-my-list 1 2 3 4) :from-end t) '(1 (2 (3 4))) "reduce")
(is (reduce #'list (make-my-list 1 2 3 4) :initial-value 'foo) '((((foo 1) 2) 3) 4) "reduce")
(is (reduce #'list (make-my-list 1 2 3 4)
            :from-end t :initial-value 'foo)
    '(1 (2 (3 (4 foo))))
    "reduce")
(is (reduce #'* (make-my-vector 1 2 3 4 5)) 120
    "reduce")
(is (reduce #'append (make-my-vector '(1) '(2)) :initial-value '(i n i t))
    '(I N I T 1 2)
    "reduce")
(is (reduce #'append (make-my-vector '(1) '(2)) :from-end t
                                                :initial-value '(i n i t))
    '(1 2 I N I T)
    "reduce")
(is (reduce #'- (make-my-vector 1 2 3 4)) -8
    "reduce")
(is (reduce #'+ (make-instance 'my-vector)) 0 "reduce")
(is (reduce #'+ (make-instance 'my-vector) :initial-value 10) 10 "reduce")
(is (reduce #'+ (make-my-vector 3)) 3 "reduce")
(is (reduce #'+ (make-my-vector 'foo)) 'foo "reduce")
(is (reduce #'list (make-my-vector 1 2 3 4)) '(((1 2) 3) 4) "reduce")
(is (reduce #'list (make-my-vector 1 2 3 4) :from-end t) '(1 (2 (3 4))) "reduce")
(is (reduce #'list (make-my-vector 1 2 3 4) :initial-value 'foo) '((((foo 1) 2) 3) 4) "reduce")
(is (reduce #'list (make-my-vector 1 2 3 4)
            :from-end t :initial-value 'foo)
    '(1 (2 (3 (4 foo))))
    "reduce")

(is (fill (list 0 1 2 3 4 5) '(444))
    '((444) (444) (444) (444) (444) (444))
    "fill")
(is (fill (copy-seq "01234") #\e :start 3) "012ee" "fill")
(let ((x (vector 'a 'b 'c 'd 'e)))
  (is (fill x 'z :start 1 :end 3)
      #('A 'Z 'Z 'D 'E)
      :test #'equalp
      "fill")
  (is (fill x 'p) #('P 'P 'P 'P 'P) :test #'equalp "fill")
  (is x #('P 'P 'P 'P 'P) :test #'equalp "fill"))

(is (coerce (fill (make-my-list 0 1 2 3 4 5) '(444)) 'list)
    '((444) (444) (444) (444) (444) (444))
    "fill")

(let ((x (make-my-vector 'a 'b 'c 'd 'e)))
  (is (coerce (fill x 'z :start 1 :end 3) 'vector)
      #('A 'Z 'Z 'D 'E)
      :test #'equalp
      "fill")
  (is (coerce (fill x 'p) 'vector) #('P 'P 'P 'P 'P) :test #'equalp "fill")
  (is (coerce x 'vector) #('P 'P 'P 'P 'P) :test #'equalp "fill"))

(is (every #'characterp "abc") t "every")
(is (some #'= '(1 2 3 4 5) '(5 4 3 2 1)) t "some")
(is (notevery #'< '(1 2 3 4) '(5 6 7 8) '(9 10 11 12)) nil "notevery")
(is (notany #'> '(1 2 3 4) '(5 6 7 8) '(9 10 11 12)) t "notany")

(is (every #'characterp (make-my-list #\a #\b #\c)) t "every")
(is (some #'= (make-my-list 1 2 3 4 5) '(5 4 3 2 1)) t "some")
(is (notevery #'< (make-my-list 1 2 3 4) '(5 6 7 8) '(9 10 11 12)) nil "notevery")
(is (notany #'> (make-my-list 1 2 3 4) '(5 6 7 8) '(9 10 11 12)) t "notany")

(is (every #'characterp (make-my-vector #\a #\b #\c)) t "every")
(is (some #'= (make-my-vector 1 2 3 4 5) '(5 4 3 2 1)) t "some")
(is (notevery #'< (make-my-vector 1 2 3 4) '(5 6 7 8) '(9 10 11 12)) nil "notevery")
(is (notany #'> (make-my-vector 1 2 3 4) '(5 6 7 8) '(9 10 11 12)) t "notany")


(use-package :cl21.lazy)

(defun fib-seq ()
  (labels ((rec (a b)
             (lazy-sequence (cons a (rec b (+ a b))))))
    (rec 0 1)))

(let ((took (take 6 (fib-seq))))
  (is-type took 'lazy-sequence "take")
  (is (coerce took 'list) '(0 1 1 2 3 5) "take"))
(is-type (drop 6 (fib-seq)) 'lazy-sequence
         "drop")
(is (coerce (take 3 (drop 6 (fib-seq))) 'list) '(8 13 21)
    "drop & take")
(let ((took (take-while (lambda (x) (< x 500)) (fib-seq))))
  (is-type took 'lazy-sequence "take-while")
  (is (coerce took 'list) '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377)))
(is-type (drop-while (lambda (x) (< x 500)) (fib-seq))
         'lazy-sequence
         "drop-while")
(is (coerce (take 3 (drop-while (lambda (x) (< x 500)) (fib-seq))) 'list)
    '(610 987 1597)
    "drop-while & take")
(ok (not (emptyp (fib-seq))) "emptyp")
(let ((sub (subseq (fib-seq) 10 13)))
  (is-type sub 'lazy-sequence "subseq")
  (is (coerce sub 'list) '(55 89 144) "subseq"))
(is (position-if (lambda (x) (> x 500)) (fib-seq))
    15
    "position-if")
(is (position-if (lambda (x) (> x 500000)) (fib-seq) :end 30)
    29
    "position-if")
(is (position-if (lambda (x) (> x 500000)) (fib-seq) :end 20)
    nil
    "position-if")
(is (find-if (lambda (x) (> x 500)) (fib-seq))
    610
    "find-if")
(is (find-if (lambda (x) (> x 500000)) (fib-seq) :end 30)
    514229
    "find-if")
(is (find-if (lambda (x) (> x 500000)) (fib-seq) :end 20)
    nil
    "find-if")

(is (map #'(lambda (x) (coerce x 'list))
         (coerce (take 3 (split-if #'oddp (fib-seq))) 'list))
    '((0) nil (2))
    "split-if")
(is (map #'(lambda (x) (coerce x 'list))
         (coerce (split-if #'oddp (fib-seq) :end 20) 'list))
    '((0) NIL (2) NIL (8) NIL (34) NIL (144) NIL (610) NIL (2584))
    "split-if")
(is (map #'(lambda (x) (coerce x 'list))
         (coerce (take 3 (split-if #'oddp (fib-seq) :count 3 :remove-empty-subseqs t)) 'list))
    '((0) (2) (8))
    "split-if")

(finalize)
