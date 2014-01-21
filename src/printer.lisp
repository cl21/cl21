(in-package :cl-user)
(defpackage cl21.printer
  (:use :cl)
  (:export :copy-pprint-dispatch
           :formatter
           :pprint-dispatch
           :pprint-exit-if-list-exhausted
           :pprint-fill
           :pprint-linear
           :pprint-tabular
           :pprint-indent
           :pprint-logical-block
           :pprint-newline
           :pprint-pop
           :pprint-tab
           :print-object
           :print-unreadable-object
           :set-pprint-dispatch
           :prin1
           :print
           :pprint
           :princ
           :write-to-string
           :prin1-to-string
           :princ-to-string
           :*print-array*
           :*print-base*
           :*print-radix*
           :*print-case*
           :*print-circle*
           :*print-escape*
           :*print-gensym*
           :*print-level*
           :*print-length*
           :*print-lines*
           :*print-miser-width*
           :*print-pprint-dispatch*
           :*print-pretty*
           :*print-readably*
           :*print-right-margin*
           :print-not-readable
           :print-not-readable-object
           :format))
