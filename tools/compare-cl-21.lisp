
(in-package :cl-user)
(defpackage :package-diff
  (:use :cl21 :cl21.core.environment :iterate)
  (:shadowing-import-from :iterate
                          :collect
                          :until
                          :collecting)
  (:shadow :while))

(in-package :package-diff)

(defun package-symbols-intersection (package1 package2)
  (let ((p1 (find-package package1))
        (p2 (find-package package2)))
    (iter 
      (for s1 in-package p1 external-only t)
      (for (values s2 status) = (find-symbol (symbol-name s1) p2))
      (when (and (eq status :external)
                 (not (eq s1 s2)))
        (collect (cons s1 s2))))))

(defun package-symbols-difference (package1 package2)
  (let ((p1 (find-package package1))
        (p2 (find-package package2)))
    (iter 
      (for s1 in-package p1 external-only t)
      (for (values s2 status) = (find-symbol (symbol-name s1) p2))
      (when (null s2)
        (collect s1)))))

(defun max-name-length (symbols)
  (reduce #'max symbols :key (compose #'length #'symbol-name)))

(defun symbol-kind (s)
  (cond
    ((special-operator-p s) :special-operator)
    ((constantp s) :constant)
    ((fboundp s)
     (if (macro-function s)
         :macro
         (typecase (symbol-function s)
           ;(standard-generic-function :standard-generic-function)
           (generic-function :generic-function)
           (function :function))))
    ((find-class s nil) :class)
    ((eq :special (variable-information 'x))
     :special-variable)
    ;; ((compiler-macro-function s) :compiler-macro)
    ))

(defun added-entry (s)
  (list s 'added
        (string-downcase
         (package-name (symbol-package s)))
        (symbol-kind s)))

(defun removed-entry (s)
  (list s 'deleted
        (string-downcase
         (package-name (symbol-package s)))
        (symbol-kind s)))

(defun changed-entry (cons)
  (destructuring-bind (s1 . s2) cons
    (list s1
          (string-downcase
           (package-name (symbol-package s1)))
          (symbol-kind s1)
          (string-downcase
           (package-name (symbol-package s2)))
          (symbol-kind s2))))

(defvar *md-align-left* ":--")
(defvar *md-align-right* "--:")
(defvar *md-align-center* ":--:")

(defun package-diff (package1 package2 &key
                     (separator " | ")
                     (stream *standard-output*))
  (flet ((next (head list)
           (%package-diff stream separator
                          (append head (sort list #'entry<))))
         (cnext (head list)
           (%package-diff stream separator
                          (append head (sort list #'centry<)))))
    (format stream "~%~%# Added/Removed symbols~%")
    (next `((:symbol :status "Imported/excluded from" :type)
            ,(make-list 4 :initial-element *md-align-left*))
          (append (map #'added-entry (package-symbols-difference package2 package1))
                  (map #'removed-entry (package-symbols-difference package1 package2))))
    (format stream "~%~%# Modified symbols~%")
    (cnext `((:symbol "Old package" :type "New package" :type)
            ,(make-list 5 :initial-element *md-align-left*))
           (map #'changed-entry (package-symbols-intersection package1 package2)))))

(defun %package-diff (s separator entries)
  (pprint-logical-block (s nil)
    (let* ((widths (vector 0 0 0 0 0 0))
           (sepl (length separator)))
      (iter (for e in entries)
            (iter (for len in (map (compose #'length #'princ-to-string) e))
                  (for w in-vector widths with-index i)
                  (setf (aref widths i)
                        (max w len))))
      (let ((tabs (iter (for i below 6)
                        (collect
                            (iter (for j to i)
                                  (summing sepl)
                                  (summing (aref widths j)))))))
        (iter (for list in entries)
              (fresh-line s)
              (princ separator s)
              (iter (for elem in list)
                    (for tab in tabs)
                    (princ elem s)
                    (pprint-tab :line tab 0 s)
                    (princ separator s)))))))


(defun entry< (e1 e2)
  (destructuring-bind (s1 act1 pkgname1 kind1) e1
    (destructuring-bind (s2 act2 pkgname2 kind2) e2
      (or (string< act1 act2)
          (and (string= act1 act2)
               (or (string< pkgname1 pkgname2)
                   (and (string= pkgname1 pkgname2)
                        ;; symbols are string designators...
                        (or (string< kind1 kind2)
                            (and (string= kind1 kind2)
                                 (string< s1 s2))))))))))


(defun centry< (e1 e2)
  (destructuring-bind (s1 pkgname1 kind1 . rest1) e1
    (destructuring-bind (s2 pkgname2 kind2 . rest2) e2
      (or (string< pkgname1 pkgname2)
          (and (string= pkgname1 pkgname2)
               ;; symbols are string designators...
               (or (string< kind1 kind2)
                   (and (string= kind1 kind2)
                        (destructuring-bind (pkgname1 kind1) rest1
                          (destructuring-bind (pkgname2 kind2) rest2
                            (or (string< pkgname1 pkgname2)
                                (and (string= pkgname1 pkgname2)
                                     ;; symbols are string designators...
                                     (or (string< kind1 kind2)
                                         (and (string= kind1 kind2)
                                              (string< s1 s2))))))))))))))


(defun output ()
  "write the comparison"
  (with-open-file (s
                   (merge-pathnames
                    "CHANGES_AUTO.markdown"
                    (asdf:system-source-directory :cl21))
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (multiple-value-bind  (second minute hour date month year)
        (decode-universal-time (get-universal-time))
      (format s "This file is automatically generated by tools/compare-cl-21.lisp~%~
               on ~a ~a at ~a/~a/~a ~a:~a '~a ."
              (lisp-implementation-type)
              (lisp-implementation-version)
              year month date hour minute second))
    (terpri s)
    (let ((*print-case* :downcase))
      (package-diff :cl :cl21 :stream s :separator "|"))))

(output)


;; (package-diff :cl :cl21)
;; (package-diff :cl :cl21 :separators (make-list 4 :initial-element " | "))

