(defpackage cl21)
(cl:in-package :cl21)

(cl:dolist (package-name '(:cl21.core))
  (cl:let ((package (cl:find-package package-name)))
    (cl:unless package
      (cl:error "Package \"~A\" doesn't exist." package-name))
    (cl:do-external-symbols (symbol package)
      (cl:shadowing-import (cl:list symbol))
      (cl:export (cl:list symbol)))))

(cl:in-package :cl-user)
(cl21::defpackage cl21-user
  (:use :cl21))
(cl21::in-package :cl21-user)
