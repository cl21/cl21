(in-package :cl-user)
(defpackage cl21.core.file
  (:use :cl)
  (:export :pathname
           :pathnamep
           :make-pathname
           :pathname-host
           :pathname-device
           :pathname-directory
           :pathname-name
           :pathname-type
           :pathname-version
           :load-logical-pathname-translations
           :logical-pathname
           :logical-pathname-translations
           :*default-pathname-defaults*
           :namestring
           :file-namestring
           :directory-namestring
           :host-namestring
           :enough-namestring
           :parse-namestring
           :wild-pathname-p
           :pathname-match-p
           :translate-logical-pathname
           :translate-pathname
           :merge-pathnames

           ;; I think these symbols should be out of the core package.
           ;; These are here only because COMMON-LISP has these.
           :directory
           :probe-file
           :ensure-directories-exist
           :truename
           :file-author
           :file-write-date
           :rename-file
           :delete-file
           :file-error
           :file-error-pathname))
