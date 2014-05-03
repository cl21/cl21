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
  ;; undocumented behavior, should be removed
  ;; (:method ((place list) (key integer) &optional default)
  ;;   (let ((res (cl:nthcdr key place)))
  ;;    (if res
  ;;        (values (car res) t)
  ;;        (values default nil))))
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
  ;; undocumented behavior, should be removed
  ;; (:method (val (place list) (key integer))
  ;;   (setf (cl:nth key place) val))

  ;; wrong when the place is a constant NIL.
  ;; (:method (val (place list) key)
  ;;   (setf (cl:getf place key) val))

  ;; wrong again when the key is not found
  ;; because (setf (getf ...)) does not modify the place in such cases
  ;; (:method (val (place cons) key)
  ;;   (setf (cl:getf place key) val))

  (:method (new-value (place cons) key)
    ;; partly borrowed from sbcl (setf getf) implementation %putf
    (do ((prev nil plist)
         (plist place (cddr plist)))
        ((endp plist)
         (setf (cddr prev) (list key new-value))
         place) ;; this part is changed
      (declare (type list plist prev))
      (when (eq (car plist) key)
        (setf (cadr plist) new-value)
        (return place)))))

;; sbcl original putf
;; (defun %putf (place property new-value)
;;   (declare (type list place))
;;   (do ((plist place (cddr plist)))
;;       ((endp plist) (list* property new-value place))
;;     (declare (type list plist))
;;     (when (eq (car plist) property)
;;       (setf (cadr plist) new-value)
;;       (return place))))


(define-setf-expander getf (place key &environment env)
  (multiple-value-bind (dummies vals newvals setter getter)
      (get-setf-expansion place env)
    (assert (= 1 (length newvals)))
    (let ((newval (first newvals)))
      (values dummies
              vals
              newvals
              `(typecase ,getter
                 (null (let ((,newval (list ,key ,newval))) ,setter))
                 (t (funcall #'(setf getf) ,newval ,getter ,key)))
              `(getf ,getter ,key)))))

(defgeneric coerce (object output-type-spec)
  (:method ((object t) output-type-spec)
    (cl:coerce object output-type-spec)))

(defmethod coerce ((object number) output-type-spec)
  (case output-type-spec
    (string (write-to-string object))
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
