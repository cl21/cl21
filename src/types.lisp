(in-package :cl-user)
(defpackage cl21.types
  (:use :cl)
  (:import-from :trivial-types
                :proper-list-p
                :proper-list
                :property-list-p
                :property-list
                :association-list-p
                :association-list

                :character-designator
                :function-designator
                :file-position-designator
                :list-designator
                :package-designator
                :pathname-designator
                :stream-designator
                :string-designator

                :file-associated-stream-p
                :file-associated-stream

                :non-nil)
  (:export :deftype
           :type-of
           :type-error
           :type-error-datum
           :type-error-expected-type
           :simple-type-error

           :structure
           :structure-class
           :structure-object

           :compiled-function

           :proper-list-p
           :proper-list
           :plist
           :plistp
           :alist
           :alistp

           :octet

           :boolean
           :t
           :nil

           :satisfies

           :character-designator
           :function-designator
           :file-position-designator
           :list-designator
           :package-designator
           :pathname-designator
           :stream-designator
           :string-designator

           :file-associated-stream-p
           :file-associated-stream))
(in-package :cl21.types)

(loop for (alias . original) in '((plistp . property-list-p)
                                  (alistp . association-list-p))
      do (setf (symbol-function alias) (symbol-function original)))

(deftype octet (&optional (n 1))
  "A type representing an octet: 8 bits. If a positive integer `n` is
specified, then `(octet n)` represents `8n` bits. This can often be
used for optimization in Common Lisp."
  (check-type n (integer 1))
  `(unsigned-byte ,(* 8 n)))
