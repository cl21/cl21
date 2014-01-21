(in-package :cl-user)
(defpackage cl21.package
  (:use :cl)
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
