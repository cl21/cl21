(in-package :cl-user)
(defpackage cl21.asdf
  (:use :cl)
  (:import-from :cl21
                :*standard-readtable*)
  (:import-from :cl21.core.package
                :cl21-available-p)
  (:import-from :named-readtables
                :find-readtable)
  (:export :cl21-system
           :cl21-source-file))
(defpackage cl21-asdf-user
  (:use :cl21))
(in-package :cl21.asdf)

;; Ensure Common Lisp files load in CL-USER package with a standard readtable.
#.`(progn
     ,@(loop
         for op in '(asdf:load-source-op asdf:compile-op)
         collect
         `(defmethod asdf:perform :around ((op ,op) (file asdf:cl-source-file))
            (if (and (not (typep file 'cl21-source-file))
                     (cl21-available-p (package-name *package*)))
                (let ((*package* (find-package :cl-user))
                      (*readtable* (if (eq *readtable*
                                           (find-readtable (intern (package-name *package*) :keyword)))
                                       #+sbcl sb-impl::*standard-readtable*
                                       #+ccl ccl::%initial-readtable%
                                       #-(or sbcl ccl) (copy-readtable ())
                                       *readtable*)))
                  (call-next-method))
                (call-next-method)))))


;; ASDF extension for projects using CL21
;;
;; Example:
;;
;;   (defsystem myapp
;;     :defsystem-depends-on (:cl21)
;;     :class :cl21-system
;;     :components ((:file "src/myapp")))

(defvar *shared-readtable* cl21:*standard-readtable*)

(defclass cl21-source-file (asdf:cl-source-file) ()
  (:documentation "Same as ASDF:CL-SOURCE-FILE except it will be loaded in CL21-USER package with CL21:*STANDARD-READTABLE*."))

(defclass cl21-system (asdf:system) ()
  (:documentation "Same as ASDF:SYSTEM except the :DEFAULT-COMPONENT-CLASS will be CL21-SOURCE-FILE."))

(defmethod update-instance-for-different-class ((previous asdf:system) (current cl21-system) &key)
  (unless (asdf::module-default-component-class current)
    (setf (asdf::module-default-component-class current)
          'cl21-source-file)))

(defmethod initialize-instance :after ((system cl21-system) &key)
  (unless (asdf::module-default-component-class system)
    (setf (asdf::module-default-component-class system)
          'cl21-source-file)))

#.`(progn
     ,@(loop
         for op in '(asdf:load-source-op asdf:compile-op)
         collect
         `(defmethod asdf:perform :around ((op ,op) (file cl21-source-file))
            (let ((*package* (find-package :cl21-asdf-user))
                  (*readtable* *shared-readtable*)
                  (*print-readably* nil))
              (call-next-method)))))

(import '(cl21-system cl21-source-file) :asdf)
