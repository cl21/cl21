(in-package :cl-user)
(defpackage cl21-test
  (:use :cl
        :cl-test-more))
(in-package :cl21-test)

(plan 956)

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
    set
    mapl
    mapc
    mapcar
    map-into
    ed
    room
    dribble
    *modules*))

(let ((cl21 (find-package :cl21)))
  (let (symbols)
    (do-external-symbols (symbol (find-package :cl))
      (unless (member symbol *ignore-symbols*)
        (if (and (boundp symbol)
                 (not (typep symbol 'boolean))
                 (not (member symbol '(+ ++ +++ - / // ///)))
                 (not (char= #\* (aref (symbol-name symbol) 0))))
            (push (intern (format nil "+~A+" symbol)) symbols)
            (push symbol symbols))))
    (dolist (symbol (sort symbols #'string<))
      (ok (eq (nth-value 1 (intern (string symbol) cl21))
              :external)
          (format nil "~A is exported." symbol)))))

(finalize)
