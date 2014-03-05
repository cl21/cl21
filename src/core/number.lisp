(in-package :cl-user)
(defpackage cl21.core.number
  (:use :cl)
  (:import-from :alexandria
                :define-constant)
  (:export :number
           :complex
           :real
           :float
           :short-float
           :single-float
           :double-float
           :long-float
           :rational
           :ratio
           :integer
           :signed-byte
           :unsigned-byte
           :mod
           :bit
           :fixnum
           :bignum
           :=
           :/=
           :<
           :>
           :<=
           :>=
           :max
           :min
           :minusp
           :plusp
           :zerop
           :floor
           :ffloor
           :ceiling
           :fceiling
           :truncate
           :ftruncate
           :round
           :fround
           :sin
           :cos
           :tan
           :asin
           :acos
           :atan
           :sinh
           :cosh
           :tanh
           :asinh
           :acosh
           :atanh
           :*
           :+
           :-
           :/
           :1+
           :1-
           :abs
           :evenp
           :oddp
           :exp
           :expt
           :gcd
           :incf
           :decf
           :lcm
           :log
           :mod
           :rem
           :signum
           :sqrt
           :isqrt
           :random-state
           :make-random-state
           :random
           :random-state-p
           :*random-state*
           :numberp
           :cis
           :complex
           :complexp
           :conjugate
           :phase
           :realpart
           :imagpart
           :upgraded-complex-part-type
           :realp
           :numerator
           :denominator
           :rational
           :rationalize
           :rationalp
           :ash
           :integer-length
           :integerp
           :parse-integer
           :boole
           :logand
           :logandc1
           :logandc2
           :logeqv
           :logior
           :lognand
           :lognor
           :lognot
           :logorc1
           :logorc2
           :logxor
           :logbitp
           :logcount
           :logtest
           :byte
           :byte-size
           :byte-position
           :deposit-field
           :dpb
           :ldb
           :ldb-test
           :mask-field
           :decode-float
           :scale-float
           :float-radix
           :float-sign
           :float-digits
           :float-precision
           :integer-decode-float
           :float
           :floatp
           :arithmetic-error
           :arithmetic-error-operands
           :arithmetic-error-operation
           :division-by-zero
           :floating-point-invalid-operation
           :floating-point-inexact
           :floating-point-overflow
           :floating-point-underflow

           :+pi+
           :+ii+
           :+ee+

           :+most-positive-fixnum+
           :+most-negative-fixnum+

           :+boole-1+
           :+boole-2+
           :+boole-and+
           :+boole-andc1+
           :+boole-andc2+
           :+boole-c1+
           :+boole-c2+
           :+boole-clr+
           :+boole-eqv+
           :+boole-ior+
           :+boole-nand+
           :+boole-nor+
           :+boole-orc1+
           :+boole-orc2+
           :+boole-set+
           :+boole-xor+

           :+most-positive-short-float+
           :+least-positive-short-float+
           :+least-positive-normalized-short-float+
           :+most-positive-double-float+
           :+least-positive-double-float+
           :+least-positive-normalized-double-float+
           :+most-positive-long-float+
           :+least-positive-long-float+
           :+least-positive-normalized-long-float+
           :+most-positive-single-float+
           :+least-positive-single-float+
           :+least-positive-normalized-single-float+
           :+most-negative-short-float+
           :+least-negative-short-float+
           :+least-negative-normalized-short-float+
           :+most-negative-single-float+
           :+least-negative-single-float+
           :+least-negative-normalized-single-float+
           :+most-negative-double-float+
           :+least-negative-double-float+
           :+least-negative-normalized-double-float+
           :+most-negative-long-float+
           :+least-negative-long-float+
           :+least-negative-normalized-long-float+
           :+short-float-epsilon+
           :+short-float-negative-epsilon+
           :+single-float-epsilon+
           :+single-float-negative-epsilon+
           :+double-float-epsilon+
           :+double-float-negative-epsilon+
           :+long-float-epsilon+
           :+long-float-negative-epsilon+))
(in-package :cl21.core.number)

(define-constant +pi+ pi
  :documentation #.(documentation 'pi 'variable))

(define-constant +ii+ #C(0 1)
  :documentation "The imaginary number `i = sqrt(-1)`.")

(define-constant +ee+ (exp 1.0d0)
  :documentation "The exponential number `e = 2.71828...`.")

(define-constant +most-negative-fixnum+
  most-negative-fixnum
  :documentation #.(documentation 'most-negative-fixnum 'variable))

(define-constant +most-positive-fixnum+
  most-positive-fixnum
  :documentation #.(documentation 'most-positive-fixnum 'variable))

#.`(progn
     ,@(loop for var in '(boole-1
                          boole-2
                          boole-and
                          boole-andc1
                          boole-andc2
                          boole-c1
                          boole-c2
                          boole-clr
                          boole-eqv
                          boole-ior
                          boole-nand
                          boole-nor
                          boole-orc1
                          boole-orc2
                          boole-set
                          boole-xor

                          most-positive-short-float
                          least-positive-short-float
                          least-positive-normalized-short-float
                          most-positive-double-float
                          least-positive-double-float
                          least-positive-normalized-double-float
                          most-positive-long-float
                          least-positive-long-float
                          least-positive-normalized-long-float
                          most-positive-single-float
                          least-positive-single-float
                          least-positive-normalized-single-float
                          most-negative-short-float
                          least-negative-short-float
                          least-negative-normalized-short-float
                          most-negative-single-float
                          least-negative-single-float
                          least-negative-normalized-single-float
                          most-negative-double-float
                          least-negative-double-float
                          least-negative-normalized-double-float
                          most-negative-long-float
                          least-negative-long-float
                          least-negative-normalized-long-float

                          short-float-epsilon
                          short-float-negative-epsilon
                          single-float-epsilon
                          single-float-negative-epsilon
                          double-float-epsilon
                          double-float-negative-epsilon
                          long-float-epsilon
                          long-float-negative-epsilon)
             collect `(define-constant ,(intern (format nil "+~A+" var))
                        ,var :documentation ,(documentation var 'variable))))
