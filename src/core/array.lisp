(in-package :cl-user)
(defpackage cl21.core.array
  (:use :cl)
  (:shadow :vector)
  (:import-from :alexandria
                :copy-array
                :define-constant)
  (:shadowing-import-from :cl21.core.generic
                          :coerce)
  (:export :array
           :simple-array
           :vector
           :simple-vector
           :bit-vector
           :simple-bit-vector
           :make-array
           :adjust-array
           :adjustable-array-p
           :aref
           :array-dimension
           :array-dimensions
           :array-element-type
           :array-has-fill-pointer-p
           :array-displacement
           :array-in-bounds-p
           :array-rank
           :array-row-major-index
           :array-total-size
           :arrayp
           :fill-pointer
           :row-major-aref
           :upgraded-array-element-type
           :simple-vector-p
           :svref
           :vector
           :vector-pop
           :vector-push
           :vector-push-extend
           :vectorp
           :bit
           :sbit
           :bit-and
           :bit-andc1
           :bit-andc2
           :bit-eqv
           :bit-ior
           :bit-nand
           :bit-nor
           :bit-not
           :bit-orc1
           :bit-orc2
           :bit-xor
           :bit-vector-p
           :simple-bit-vector-p

           ;; Variables
           :+array-dimension-limit+
           :+array-rank-limit+
           :+array-total-size-limit+

           :copy-array))
(in-package :cl21.core.array)

(define-constant +array-dimension-limit+
  array-dimension-limit
  :documentation #.(documentation 'array-dimension-limit 'variable))

(define-constant +array-rank-limit+
  array-rank-limit
  :documentation #.(documentation 'array-rank-limit 'variable))

(define-constant +array-total-size-limit+
  array-total-size-limit
  :documentation #.(documentation 'array-total-size-limit 'variable))

(deftype vector (&optional (type t) (dimention-spec '*))
  `(cl:vector ,type ,dimention-spec))

(defun vector (&rest objects)
  (let ((vec (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for obj in objects
          do (vector-push-extend obj vec))
    vec))

;; defining a new 'atomic type specifier' is not possible in common lisp,
;; except it is defined by defstruct and defclass.
(defmethod coerce ((object t) (output-type-spec (eql 'vector)) &key)
  (cl:coerce object 'cl:vector))

(defmacro adjustable-vector (&key (dimension nil dimension-specified-p) initial-contents)
  "A variant of `cl:vector' that returns an adjustable vector, not a simple-vector."
  (let ((len (length initial-contents)))
    (if dimension-specified-p
        (when (> dimension len)
          (setf initial-contents
                (append initial-contents
                        (loop with initial-element = (car (last initial-contents))
                              repeat (- dimension len)
                              collect initial-element))))
        (setf dimension len))

    `(make-array ,dimension
                 :adjustable t :fill-pointer ,dimension
                 :initial-contents (list ,@initial-contents))))
