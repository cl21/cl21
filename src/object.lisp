(in-package :cl-user)
(defpackage cl21.object
  (:use :cl)
  (:export :function-keywords
           :ensure-generic-function
           :allocate-instance
           :initialize-instance
           :reinitialize-instance
           :shared-initialize
           :update-instance-for-different-class
           :update-instance-for-redefined-class
           :change-class
           :slot-boundp
           :slot-exists-p
           :slot-makunbound
           :slot-missing
           :slot-unbound
           :slot-value
           :method-qualifiers
           :no-applicable-method
           :no-next-method
           :remove-method
           :make-instance
           :make-instances-obsolete
           :make-load-form
           :make-load-form-saving-slots
           :with-accessors
           :with-slots
           :defclass
           :defgeneric
           :defmethod
           :find-class
           :next-method-p
           :call-method
           :make-method
           :call-next-method
           :compute-applicable-methods
           :define-method-combination
           :find-method
           :add-method
           :class-name
           :class-of
           :unbound-slot
           :unbound-slot-instance
           :standard))
