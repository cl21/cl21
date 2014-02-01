(in-package :cl-user)
(defpackage cl21.core.string-designator
  (:use :cl)
  (:export :upcase
           :downcase))
(in-package :cl21.core.string-designator)

(defgeneric upcase (object &key start end)
  (:method ((object string) &key (start 0) end)
    (string-upcase object :start start :end end))
  (:method ((object symbol) &key (start 0) end)
    (nth-value 0
               (intern (string-upcase object :start start :end end)
                       (symbol-package object)))))

(defgeneric downcase (object &key start end)
  (:method ((object string) &key (start 0) end)
    (string-downcase object :start start :end end))
  (:method ((object symbol) &key (start 0) end)
    (nth-value 0
               (intern (string-downcase object :start start :end end)
                       (symbol-package object)))))
