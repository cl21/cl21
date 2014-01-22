(in-package :cl-user)
(defpackage cl21.core.string-designator
  (:use :cl)
  (:export :concat
           :upcase
           :downcase))
(in-package :cl21.core.string-designator)

(defgeneric concat (seq &rest sequences)
  (:method ((seq list) &rest sequences)
    (apply #'append seq sequences))
  (:method ((seq string) &rest sequences)
    (apply #'concatenate 'string seq sequences))
  (:method ((seq vector) &rest sequences)
    (apply #'concatenate 'vector seq sequences)))

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
