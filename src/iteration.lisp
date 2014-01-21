(in-package :cl-user)
(defpackage cl21.iteration
  (:use :cl)
  (:import-from :alexandria
                :once-only)
  (:import-from :cl-utilities
                :with-collectors
                :collecting
                :collect)
  (:export :loop
           :do
           :do*
           :dotimes
           :dolist
           :loop-finish

           :with-collectors
           :collecting
           :collect

           :until
           :while
           :while-let
           :doeach))
(in-package :cl21.iteration)

(defmacro until (expression &body body)
  "Executes `body` until `expression` is true."
  `(do ()
       (,expression)
     ,@body))

(defmacro while (expression &body body)
  "Executes `body` while `expression` is true."
  `(until (not ,expression)
     ,@body))

(defmacro while-let ((varsym expression) &body body)
  `(let (,varsym)
     (while (setf ,varsym ,expression)
       ,@body)))

(defmacro doeach ((varsym seq) &body body)
  (once-only (seq)
    `(etypecase ,seq
       (list (dolist (,varsym ,seq)
               ,@body))
       (sequence (loop for ,varsym across ,seq
                       do (progn ,@body))))))
