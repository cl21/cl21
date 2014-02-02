# CL21 - Common Lisp in the 21st Century.

This is an experimental project redesigning Common Lisp.

## Usage

```common-lisp
(in-package :cl21-user)
(defpackage myapp
  (:use :cl21))
(in-package :myapp)


;;
;; Hello, World!

(princ "Hello, World!\n")
;-> Hello, World!
;=> "Hello, World!
;   "

(let ((name "John"))
  (princ "Hello, ${name}!"))
;-> Hello, John!
;=> "Hello, John!"


;;
;; Hash Table

(defvar *hash* #{})

(getf *hash* :name)
;=> NIL

(setf (getf *hash* :name) "Eitarow Fukamachi")
;=> "Eitarow Fukamachi"
(setf (getf *hash* :living) "Japan")
;=> "Japan"

(getf *hash* :name)
;=> "Eitarow Fukamachi"

(coerce *hash* 'plist)
;=> (:LIVING "Japan" :NAME "Eitarow Fukamachi")


;;
;; Vector

(defvar *vector* #())

(push 1 *vector*)
(nth 0 *vector*)
;=> 1

(push 3 *vector*)
(nth 1 *vector*)
;=> 3

(pop *vector*)
;=> 3
(pop *vector*)
;=> 1


;;
;; Iteration
;;
;;   `doeach` is similar to `dolist`, but it can be used with all sequences.
;;

(collecting
  (doeach (x '("al" "bob" "joe"))
    (when (> (length x) 2)
      (collect x))))
;=> ("bob" "joe")


;;
;; Anonymous Function

(mapcar #'(list (1+ _)) '(1 2 3))
;=> ((2) (3) (4))

;;
;; Regular Expression

(use-package :cl21.re)

;; #/.../ is a literal regular expression.

(re-match #/^Hello, (.+?)!$/ "Hello, World!")
;=> "Hello, World!"
;   #("World")

;; Regular expression can be called like a function.

(#/^(\d{4})-(\d{2})-(\d{2})$/ "2014-01-23")
;=> "2014-01-23"
;   #("2014" "01" "23")

(re-replace #/a/g "Eitarow Fukamachi" "α")
;=> "Eitαrow Fukαmαchi"
;   T
```

## Features

* Can completely cooperate with existing Common Lisp applications/libraries.
* More object oriented.
* Add more functional programming facilities.
* Organize symbols into several packages.
* MOP.
* Regular expression.
* Package local nicknames.

### Deferred List

* Threads.
* POSIX.
* Functions to run shell commands.

## Requirements

CL21 is written in pure Common Lisp and intended to run on a Common Lisp implementation.

It is tested on the latest version of SBCL, Clozure CL, GNU CLISP and Allegro CL.

## Installation

```common-lisp
(ql-dist:install-dist "http://qldists.8arrow.org/cl21.txt")
(ql:quickload :cl21)
```

## Update to the latest version

CL21 is continuously released at 1:00 JST (= 16:00 UTC). You can update to the HEAD version by this command.

```common-lisp
(ql:update-dist "cl21")
```

## Added Symbols

### Core
* emptyp

### Control Structure
* if-let
* when-let
* xor
* unwind-protect-case

### Iteration
* until
* while
* while-let
* doeach
* doplist
* with-collectors
* collecting

### Package
* package-readtable
* add-package-local-nickname

### Number
* imaginary-i
* ii
* exponential-e
* ee

### Symbol
* make-keyword

### Function
* compose
* conjoin
* disjoin
* curry
* rcurry

### Sequence
* concat
* upcase
* downcase
* split-sequence
* split-sequence-if
* copy-array
* length=
* maptree
* take
* drop
* take-while
* drop-while
* partition-by

### List
* iota
* 1..
* 0..
* remove-from-plist
* delete-from-plist
* ensure-list
* flatten
* mappend
* maptree
* filter (Same as `remove-if-not`)
* list-nth (Same as `cl:nth`)
* list-push (Same as `cl:push`)
* list-pushnew (Same as `cl:pushnew`)
* list-pop (Same as `cl:pop`)

### Stream
* make-null-stream
* make-null-input-stream
* make-null-output-stream
* copy-stream
* read-file-into-string
* write-string-into-file
* read-file-into-byte-vector
* write-byte-vector-into-file

### Hash Table
* hash-table-keys
* hash-table-values
* hash-table-key-exists-p
* copy-hash-table
* plist-hash-table
* alist-hash-table

### MOP
All external symbols of Closer MOP.

### Macro Writing
* once-only
* with-gensyms

### Types
* proper-list
* proper-list-p
* plist
* plistp
* alist
* alistp
* character-designator
* function-designator
* file-position-designator
* list-designator
* package-designator
* pathname-designator
* stream-designator
* string-designator
* file-associated-stream
* file-associated-stream-p
* octet

### Development Utilities
* doc
* readme
* summary

### Regular Expression (cl21.re)
* re-match
* re-replace
* re-split

## Deleted Symbols
* prog2
* find-if-not
* position-if-not
* member-if-not
* substitute-if-not
* nsubstitute-if-not
* subst-if-not
* nsubst-if-not
* assoc-if-not
* rassoc-if-not
* count-if-not
* gentemp
* provide
* set
* \*modules\*

## Deprecated Symbols
* remove-if-not (Use `filter`)

## Redefined
* equalp
* nth
* nthcdr
* coerce
* getf
* push
* pushnew
* pop
* defpackage
* in-package
* find-package
* copy-readtable

## Setting the startup package of SLIME

1) Load CL21 in your Lisp init file.

```common-lisp
(ql:quickload :cl21)
```

2) Add this to you .emacs.el.

```common-lisp
(add-hook 'slime-connected-hook (lambda ()
                                  (when (slime-eval `(cl:if (cl:find-package :cl21-user) t))
                                    (slime-repl-set-package :cl21-user))) t)
```

## See Also

* Closer MOP
* Trivial Types
* Alexandria
* CL-Utilities
* Split-Sequence
* REPL-Utilities

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)

# License

Licensed under the MIT License.
