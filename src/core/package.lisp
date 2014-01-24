(in-package :cl-user)
(defpackage cl21.core.package
  (:use :cl)
  (:shadow :in-package)
  (:import-from :named-readtables
                :find-readtable
                :in-readtable
                :reader-macro-conflict)
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

(defmacro in-package (name)
  `(prog1
       (cl:in-package ,name)
     ,(let ((package-readtable (intern #.(string :package-readtable) name)))
        `(if (find-readtable ',package-readtable)
             (handler-bind ((reader-macro-conflict
                              (lambda (condition)
                                (let ((continue (find-restart 'continue condition)))
                                  (when continue
                                    (invoke-restart continue))))))
               (in-readtable ,package-readtable))
             (in-readtable nil)))))
