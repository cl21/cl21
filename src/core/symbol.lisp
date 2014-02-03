(in-package :cl-user)
(defpackage cl21.core.symbol
  (:use :cl)
  (:import-from :alexandria
                :make-keyword
                :symbolicate
                :with-gensyms)
  (:export :symbol
           :keyword
           :symbolp
           :keywordp
           :make-symbol
           :make-keyword
           :copy-symbol
           :gensym
           :*gensym-counter*
           :symbol-function
           :symbol-name
           :symbol-package
           :symbol-plist
           :symbol-value
           :get
           :remprop
           :boundp
           :makunbound
           :unbound-variable

           :symbolicate
           :with-gensyms))
