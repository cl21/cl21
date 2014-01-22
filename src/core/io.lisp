(in-package :cl-user)
(defpackage cl21.core.io
  (:use :cl)
  (:import-from :alexandria
                :with-input-from-file
                :with-output-to-file
                :read-file-into-string
                :write-string-into-file
                :read-file-into-byte-vector
                :write-byte-vector-into-file
                :copy-file)
  (:export :with-input-from-file
           :with-output-to-file
           :read-file-into-string
           :write-string-into-file
           :read-file-into-byte-vector
           :write-byte-vector-into-file
           :copy-file))
