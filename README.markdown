# CL21 - Common Lisp in 21th Century.

This is an experimental project redesigning Common Lisp.

## Usage

```common-lisp
(defpackage myapp
  (:use :cl21))
(in-package :myapp)

;;
;; Hash Table

(defvar *hash* (make-hash-table))

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

(defparameter *vector*
  (make-array 0 :adjustable t :fill-pointer 0))

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
```

## Features

* More object oriented.
* Add more functional programming facilities.
* Organize symbols into several packages.
* MOP.

### Deferred List

* Threads.
* POSIX.
* Functions to run shell commands.
* Regular expression.

## Added Symbols

### Core
* emptyp

### Control Structure
* if-let
* when-let
* xor
* destructuring-case
* unwind-protect-case

### Iteration
* until
* while
* while-let
* doeach
* with-collectors
* collecting

### Number
* imaginary-i
* ii
* exponential-e
* ee

### Symbol
* make-keyword

### Function
* compose
* curry
* rcurry

### Sequence
* concat
* split-sequence
* split-sequence-if
* copy-array

### List
* iota
* remove-from-plist
* delete-from-plist
* ensure-list
* filter (Same as `remove-if-not`)

### Stream
* make-null-stream
* make-null-input-stream
* make-null-output-stream
* copy-stream

### Hash Table
* hash-table-keys
* hash-table-values
* hash-table-key-exists-p
* copy-hash-table
* plist-hash-table
* alist-hash-table

### I/O
* with-input-from-file
* with-output-to-file
* read-file-into-string
* write-string-into-file
* read-file-into-byte-vector
* write-byte-vector-into-file
* copy-file

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

## Deprecated Symbols
* elt (Use new `nth`)
* concatenate (Use `concat`)
* dolist (Use `doeach`)
* gethash (Use new `getf`)
* slot-value (Use new `getf`)
* remove-if-not (Use `filter`)
* vector-push & vector-push-extend (Use new `push`)
* vector-pop (Use new `pop`)

## Redefined Classes
* hash-table

## Redefined as Generic functions
* equal
* equalp
* nth
* first
* second
* third
* fourth
* fifth
* sixth
* seventh
* eighth
* ninth
* tenth
* last
* coerce
* getf
* remf
* gethash
* remhash
* maphash
* clrhash
* hash-table-count
* push
* pushnew

## Setting the startup package of SLIME

1. Load CL21 in your Lisp init file.

```common-lisp
(ql:quickload :cl21)
```

2. Add this to you .emacs.el.

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
