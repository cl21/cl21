(in-package :cl-user)
(defpackage cl21.generic
  (:use :cl)
  (:shadow :equal
           :equalp
           :getf
           :coerce
           :push
           :pushnew
           :pop
           :remf)
  (:shadowing-import-from :cl21.mop
                          :standard-class)
  (:import-from :cl21.types
                :plist
                :alist)
  (:export :concat
           :equal
           :equalp
           :emptyp
           :getf
           :coerce
           :upcase
           :downcase
           :push
           :pushnew
           :pop
           :remf))
(in-package :cl21.generic)

(defgeneric equal (x y)
  (:method (x y)
    (cl:equal x y)))

(defgeneric equalp (x y)
  (:method (x y)
    (cl:equalp x y)))

(defgeneric concat (seq &rest sequences)
  (:method ((seq list) &rest sequences)
    (apply #'append seq sequences))
  (:method ((seq string) &rest sequences)
    (apply #'concatenate 'string seq sequences))
  (:method ((seq vector) &rest sequences)
    (apply #'concatenate 'vector seq sequences)))

(defgeneric emptyp (object)
  (:method ((x null)) t)
  (:method ((x cons)) nil)
  (:method ((x vector)) (zerop (length x)))
  (:method ((x array)) (notany #'(lambda (n)
                                   (not (zerop n))) (array-dimensions x)))
  (:method ((x hash-table)) (zerop (hash-table-count x))))

(defgeneric getf (place key &optional default)
  (:method ((place hash-table) key &optional default)
    (nth-value 0 (gethash key place default)))
  (:method ((place standard-object) key &optional default)
    (if (slot-boundp place key)
        (slot-value place key)
        default))
  (:method ((place list) key &optional default)
    (cl:getf place key default)))

(defgeneric (setf getf) (val place key)
  (:method (val (place hash-table) key)
    (setf (gethash key place) val))
  (:method (val (place standard-object) key)
    (setf (slot-value place key) val))
  (:method (val (place list) key)
    (setf (cl:getf place key) val)))

(defgeneric coerce (object output-type-spec)
  (:method ((object t) output-type-spec)
    (cl:coerce object output-type-spec)))

(defmethod coerce ((object hash-table) output-type-spec)
  (ecase output-type-spec
    (plist
     (alexandria:hash-table-plist object))
    (alist
     (alexandria:hash-table-alist object))))

(defmethod coerce ((object string) output-type-spec)
  (ecase output-type-spec
    (integer (parse-integer object :junk-allowed t))
    (symbol  (intern string))
    (keyword (intern string :keyword))))

(defmethod coerce ((object symbol) output-type-spec)
  (ecase output-type-spec
    (string (symbol-name object))))

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

(defgeneric push (value place)
  (:method (value place)
    (cl:push value place))
  (:method (value (place vector))
    (vector-push-extend value place)))

(defgeneric pushnew (value place &key key test test-not)
  (:method (value place &key key test test-not)
    (cl:pushnew value place
                :key key
                :test test
                :test-not test-not))
  (:method (value (place vector) &key key test test-not)
    (or (find value place :key key :test test :test test-not)
        (vector-push-extend place value))))

(defgeneric pop (place)
  (:method (place)
    (cl:pop place))
  (:method ((place vector))
    (cl:vector-pop place)))

(defgeneric remf (place indicator)
  (:method (place indicator)
    (cl:remf place indicator)))
