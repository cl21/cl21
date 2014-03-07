(in-package :cl-user)
(defpackage cl21.core.types
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
                :file-associated-stream)
  (:shadowing-import-from :c2mop
                          :class
                          :built-in-class
                          :slot-definition
                          :direct-slot-definition
                          :effective-slot-definition
                          :specializer
                          :eql-specializer
                          :metaobject
                          :method
                          :method-combination
                          :standard-accessor-method
                          :standard-class
                          :standard-generic-function
                          :standard-direct-slot-definition
                          :standard-effective-slot-definition
                          :standard-method
                          :standard-object
                          :standard-reader-method
                          :standard-slot-definition
                          :standard-writer-method
                          :classp
                          :subclassp
                          :typep
                          :subtypep)
  (:export :deftype
           :type-of
           :type-error
           :type-error-datum
           :type-error-expected-type
           :simple-type-error

           :classp
           :subclassp
           :typep
           :subtypep

           :compiled-function
           :octet
           :boolean

           :satisfies

           :proper-list-p
           :proper-list
           :plist
           :plistp
           :alist
           :alistp

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
(in-package :cl21.core.types)

(loop for (alias . original) in '((plistp . property-list-p)
                                  (alistp . association-list-p))
      do (setf (symbol-function alias) (symbol-function original)))

#.`(progn
     ,@(loop for (alias . original) in '((plist . property-list)
                                         (alist . association-list))
             collect `(deftype ,alias () ',original)))

(deftype octet (&optional (n 1))
  "A type representing an octet: 8 bits. If a positive integer `n` is
specified, then `(octet n)` represents `8n` bits. This can often be
used for optimization in Common Lisp."
  (check-type n (integer 1))
  `(unsigned-byte ,(* 8 n)))
