(in-package :cl-user)
(defpackage cl21.core.array
  (:use :cl)
  (:import-from :alexandria
                :copy-array)
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
           :array-dimension-limit
           :array-rank-limit
           :array-total-size-limit
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

           :copy-array))
(in-package :cl21.core.array)

(defmacro adjustable-vector (&key (dimension nil dimension-specified-p) initial-contents)
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
