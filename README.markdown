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
(elt *vector* 0)
;=> 1

(push 3 *vector*)
(elt *vector* 1)
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
;; Functional programming

(mapcar (compose #'sin #'1+) '(1 2 3))
;=> (0.9092974 0.14112 -0.7568025)

(keep-if (conjoin #'integerp #'evenp) '(1 2 3 2.0 4))
;=> (2 4)
(keep-if (disjoin #'oddp #'zerop) (0.. 10))
;=> (0 1 3 5 7 9)

;; Sharpsign quote (#') is overwritten.
(keep-if #'(and integerp evenp) '(1 2 3 2.0 4))
;=> (2 4)
(keep-if #'(and integerp (or oddp zerop)) (0.. 10))
;=> (0 1 3 5 7 9)


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


;;
;; Lazy Sequence

(use-package :cl21.lazy)

(defun fib-seq ()
  (labels ((rec (a b)
             (lazy-sequence (cons a (rec b (+ a b))))))
    (rec 0 1)))

(take 20 (fib-seq))
;=> (0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181)

(take 3 (drop-while (lambda (x) (< x 500)) (fib-seq)))
;=> (610 987 1597)
```

## Features

* Can completely cooperate with existing Common Lisp applications/libraries.
* More object oriented.
* Add more functional programming facilities.
* Organize symbols into several packages.
* Include MOP.
* Include trivial-gray-streams.
* Literal regular expression.
* Package local nicknames. (experimental)

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

### Macro Writing
* once-only

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

### Package
* package-readtable
* add-package-local-nickname

### Object
All external symbols of Closer MOP.

### Function
* compose
* conjoin
* disjoin
* curry
* rcurry

### Number
* imaginary-i
* ii
* exponential-e
* ee

### Symbol
* make-keyword
* symbolicate
* with-gensyms

### Cons
* iota
* 1..
* 0..
* remove-from-plist
* delete-from-plist
* ensure-list
* ensure-cons
* ensure-car
* flatten
* mappend
* maptree
* keep
* nkeep
* keep-if (Same as `remove-if-not`)
* nkeep-if (Same as `delete-if-not`)
* list-push (Same as `cl:push`)
* list-pushnew (Same as `cl:pushnew`)
* list-pop (Same as `cl:pop`)
* cons-last (Same as `cl:last`)

### Array
* copy-array

### Sequence
* concat
* split-sequence
* split-sequence-if
* length=
* maptree
* take
* drop
* take-while
* drop-while
* partition-if
* subdivide
* abstract-sequence
* abstract-list
* abstract-vector
* abstract-first
* abstract-rest
* abstract-cons
* abstract-vector-push
* abstract-vector-pop
* abstract-elt
* abstract-subseq
* abstract-length
* abstract-take
* abstract-drop
* abstract-take-while
* abstract-drop-while
* abstract-last
* abstract-butlast
* abstract-find-if
* abstract-find
* abstract-position-if
* abstract-position
* abstract-search
* abstract-remove-if
* abstract-remove
* abstract-partition-if
* abstract-subdivide
* abstract-mismatch
* abstract-count-if
* abstract-count
* abstract-reverse
* abstract-reduce
* abstract-sort
* abstract-stable-sort
* abstract-substitute-if
* abstract-substitute
* abstract-remove-duplicates
* abstract-split-sequence
* abstract-split-sequence-if
* abstract-member
* abstract-member-if

### Hash Table
* hash-table-keys
* hash-table-values
* hash-table-key-exists-p
* copy-hash-table
* plist-hash-table
* alist-hash-table
* abstract-hash-table
* abstract-gethash
* abstract-remhash
* abstract-clrhash
* abstract-copy-hash-table
* abstract-hash-table-count
* abstract-hash-table-rehash-size
* abstract-hash-table-rehash-threshold
* abstract-hash-table-size
* abstract-hash-table-test

### Stream
* make-null-stream
* make-null-input-stream
* make-null-output-stream
* copy-stream
* read-file-into-string
* write-string-into-file
* read-file-into-byte-vector
* write-byte-vector-into-file

And, All external symbols of trivial-gray-streams.

### Development Utilities
* doc
* readme
* summary

### Readtable
* enable-cl21-syntax
* disable-cl21-syntax
* \*standard-readtable\*
* syntax
* syntaxp
* make-syntax
* defsyntax
* syntax-name
* syntax-rules
* find-syntax
* use-syntax
* export-syntax

### CLtL2
* compiler-let
* variable-information
* function-information
* declaration-information
* augment-environment
* define-declaration
* parse-macro
* enclose

### Regular Expression (cl21.re)
* re-match
* re-replace
* re-split

### Lazy Evaluation (cl21.lazy)
* lazy-sequence

### Abbreviation (cl21.abbr)
* dbind
* mvbind
* mvcall
* mvlist
* mvsetq
* mvprog1

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
* remove-if-not (Use `keep-if`)
* delete-if-not (Use `nkeep-if`)

## Redefined
* equalp
* coerce
* getf
* push
* pushnew
* pop
* last
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
* rest
* subseq
* find
* find-if
* position
* position-if
* search
* elt
* remove
* remove-if
* remove-if-not
* delete
* delete-if
* delete-if-not
* mismatch
* length
* count
* count-if
* map
* map-into
* reverse
* nreverse
* reduce
* sort
* stable-sort
* substitute
* substitute-if
* nsubstitute
* nsubstitute-if
* member
* member-if
* remove-duplicates
* delete-duplicates
* hash-table-p
* hash-table-count
* hash-table-rehash-size
* hash-table-rehash-threshold
* hash-table-size
* hash-table-test
* gethash
* remhash
* clrhash
* maphash
* defpackage
* in-package
* find-package
* copy-readtable
* function
* destructuring-bind

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
* trivial-gray-streams
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
