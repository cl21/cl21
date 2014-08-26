(in-package :cl-user)
(defpackage cl21.ext.repl
  (:use :cl)
  (:import-from :repl-utilities
                :doc
                :readme
                :summary)
  (:export :-
           :+
           :++
           :+++
           :*
           :**
           :***
           :/
           ://
           :///

           :inspect
           :describe
           :describe-object
           :apropos
           :apropos-list

           :doc
           :readme
           :summary))
