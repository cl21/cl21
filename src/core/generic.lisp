(in-package :cl-user)
(defpackage cl21.core.generic
  (:use :cl)
  (:shadow :equalp
           :coerce
           :getf)
  (:import-from :cl21.core.types
                :plist
                :alist)
  (:import-from :alexandria
                :hash-table-plist
                :hash-table-alist)
  (:export :equalp
           :coerce
           :getf
           :emptyp))
(in-package :cl21.core.generic)

(defgeneric equalp (x y)
  (:method (x y)
    (cl:equalp x y)))

(defgeneric emptyp (object)
  (:method ((x null)) t)
  (:method ((x cons)) nil)
  (:method ((x vector)) (zerop (length x)))
  (:method ((x array)) (notany #'(lambda (n)
                                   (not (zerop n))) (array-dimensions x)))
  (:method ((x hash-table)) (zerop (hash-table-count x))))

(defgeneric getf (place key &optional default)
  (:method ((place hash-table) key &optional default)
    (gethash key place default))
  (:method ((place array) key &optional default)
    (if (< key (length place))
        (values (aref place key) t)
        (values default nil)))
  (:method ((place list) (key integer) &optional default)
    (let ((res (cl:nthcdr key place)))
     (if res
         (values (car res) t)
         (values default nil))))
  (:method ((place list) key &optional default)
    (let* ((undef '#:undef)
           (res (cl:getf place key undef)))
      (if (eq res undef)
          (values default nil)
          (values res t)))))

(defgeneric (setf getf) (val place key)
  (:method (val (place hash-table) key)
    (setf (gethash key place) val))
  (:method (val (place array) key)
    (setf (cl:aref place key) val))
  (:method (val (place list) (key integer))
    (setf (cl:nth key place) val))
  (:method (val (place list) key)
    (setf (cl:getf place key) val)))

(defgeneric coerce (object output-type-spec)
  (:method ((object t) output-type-spec)
    (cl:coerce object output-type-spec)))

(defmethod coerce ((object number) output-type-spec)
  (case output-type-spec
    (string (write-to-string object))
    (T (cl:coerce object output-type-spec))))

(defmethod coerce ((object vector) output-type-spec)
  (case output-type-spec
    (number (let ((shift (etypecase object
			   ((vector (unsigned-byte 2) *) 1)
			   ((vector (unsigned-byte 8) *) 3)
			   ((vector (unsigned-byte 16) *) 4)))
		  (result 0))
	     (loop :for bit :across object :do
		(setf result (+ (ash result shift) bit)))
	     result))
    (T (cl:coerce object output-type-spec))))

(defmethod coerce ((object hash-table) output-type-spec)
  (ecase output-type-spec
    (plist
     (alexandria:hash-table-plist object))
    (alist
     (alexandria:hash-table-alist object))))

(defmethod coerce ((object string) output-type-spec)
  (case output-type-spec
    (integer (nth-value 0 (parse-integer object :junk-allowed t)))
    ((number float) (let ((read (read-from-string object)))
                      (if (numberp read)
                          read
                          0)))
    (symbol  (intern object))
    (keyword (intern object :keyword))
    (T (cl:coerce object output-type-spec))))

(defmethod coerce ((object symbol) output-type-spec)
  (case output-type-spec
    (string (symbol-name object))
    (keyword (intern (symbol-name object) :keyword))
    (T (cl:coerce object output-type-spec))))
