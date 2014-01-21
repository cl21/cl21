(in-package :cl-user)
(defpackage cl21.environment
  (:use :cl)
  (:import-from :repl-utilities
                :doc
                :readme
                :summary)
  (:export :decode-universal-time
           :encode-universal-time
           :get-universal-time
           :get-decoded-time
           :sleep
           :apropos
           :apropos-list
           :describe
           :describe-object
           :trace
           :untrace
           :step
           :time
           :internal-time-units-per-second
           :get-internal-real-time
           :get-internal-run-time
           :disassemble
           :documentation
           :variable
           :compiler-macro
           :room
           :ed
           :inspect
           :dribble
           :-
           :+
           :++
           :+++
           :*
           :**
           :***
           :/
           ://
           :///
           :lisp-implementation-type
           :lisp-implementation-version
           :short-site-name
           :long-site-name
           :machine-instance
           :machine-type
           :machine-version
           :software-type
           :software-version
           :user-homedir-pathname

           :doc
           :readme
           :summary))
