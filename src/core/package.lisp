(in-package :cl-user)
(defpackage cl21.core.package
  (:use :cl)
  (:shadow :in-package
           :use-package
           :defpackage)
  (:import-from :cl21.core.readtable
                :get-package-readtable-options)
  (:import-from :named-readtables
                :defreadtable
                :find-readtable
                :in-readtable)
  (:import-from :alexandria
                :ensure-list)
  (:export :package
           :export
           :find-symbol
           :find-package
           :list-all-packages
           :make-package
           :rename-package
           :delete-package
           :import
           :shadow
           :shadowing-import
           :with-package-iterator
           :unexport
           :in-package
           :use-package
           :unuse-package
           :defpackage
           :find-all-symbols
           :do-all-symbols
           :intern
           :unintern
           :package-name
           :package-nicknames
           :package-shadowing-symbols
           :package-use-list
           :package-used-by-list
           :packagep
           :*package*
           :package-error
           :package-error-package
           :do-external-symbols
           :do-symbols
           :do-all-symbols))
(cl:in-package :cl21.core.package)

(defvar *package-use* (make-hash-table :test 'eq))

(defmacro defpackage (name &rest options)
  `(prog1
       (cl:defpackage ,name ,@options)
     ,@(if (member :use options :key #'car)
           `((setf (gethash ,(intern (string name) :keyword) *package-use*)
                   ',(loop for use in (cdr (assoc :use options))
                           when (keywordp use)
                             collect use
                           else
                             collect (intern (string use) :keyword))))
           nil)))

(defmacro in-package (name)
  `(prog1
       (cl:in-package ,name)
     (if (find-readtable ',name)
         (in-readtable ,name)
         ,(let ((readtable-options (loop for use in (gethash (intern (string name) :keyword) *package-use*)
                                         append (get-package-readtable-options use))))
            (if readtable-options
                `(progn
                   (defreadtable ,name
                     (:merge :standard)
                     ,@readtable-options)
                   (in-readtable ,name))
                '(in-readtable nil))))))

(defmacro use-package (packages-to-use &optional (package *package*))
  `(prog1
       (cl:use-package ,packages-to-use ,package)
     (setf (gethash ,(intern (package-name package) :keyword) *package-use*)
           (append
            ',(loop for use in (ensure-list packages-to-use)
                    when (keywordp use)
                      collect use
                    else
                      collect (intern (symbol-name use) :keyword))
            (gethash ,(intern (package-name package) :keyword) *package-use*)))
     (defreadtable ,(intern (package-name package))
       (:merge :current)
       ,@(loop for use in (ensure-list packages-to-use)
               append (get-package-readtable-options use)))
     (in-readtable ,(intern (package-name package)))))
