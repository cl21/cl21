(in-package :cl-user)
(defpackage cl21-test
  (:use :cl
        :cl-test-more))
(in-package :cl21-test)

(plan 962)

(defvar *ignore-symbols*
  '(prog2
    find-if-not
    position-if-not
    member-if-not
    substitute-if-not
    nsubstitute-if-not
    subst-if-not
    nsubst-if-not
    assoc-if-not
    rassoc-if-not
    count-if-not
    gentemp
    provide
    require
    set
    *modules*))

(let ((cl21 (find-package :cl21)))
  (let (symbols)
    (do-external-symbols (symbol (find-package :cl))
      (unless (member symbol *ignore-symbols*)
        (push symbol symbols)))
    (dolist (symbol (sort symbols #'string<))
      (ok (eq (nth-value 1 (intern (string symbol) cl21))
              :external)
          (format nil "~A is exported." symbol)))))

(finalize)
