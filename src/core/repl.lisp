(in-package :cl-user)
(defpackage cl21.core.repl
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
           :apropos
           :apropos-list

           :doc
           :readme
           :summary))
