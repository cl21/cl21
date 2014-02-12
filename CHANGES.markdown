
This is a List of Symbols which differs from standard CL.
It is maintained only manually, and some elements might not
reflect the current status.

# Added Symbols

## Core
* emptyp

## Macro Writing
* once-only

## Control Structure
* if-let
* when-let
* xor
* unwind-protect-case

## Iteration
* until
* while
* while-let
* doeach
* doplist
* with-collectors
* collecting

## Types
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

## Package
* package-readtable
* add-package-local-nickname

## Object
All external symbols of Closer MOP.

## Function
* compose
* conjoin
* disjoin
* curry
* rcurry

## Number
* imaginary-i
* ii
* exponential-e
* ee

## Symbol
* make-keyword
* symbolicate
* with-gensyms

## Cons
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

## Array
* copy-array

## Sequence
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

## Hash Table
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

## Stream
* make-null-stream
* make-null-input-stream
* make-null-output-stream
* copy-stream
* read-file-into-string
* write-string-into-file
* read-file-into-byte-vector
* write-byte-vector-into-file

And, All external symbols of trivial-gray-streams.

## Development Utilities
* doc
* readme
* summary

## Readtable
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

## CLtL2
* compiler-let
* variable-information
* function-information
* declaration-information
* augment-environment
* define-declaration
* parse-macro
* enclose

## Regular Expression (cl21.re)
* re-match
* re-replace
* re-split

## Lazy Evaluation (cl21.lazy)
* lazy-sequence

## Abbreviation (cl21.abbr)
* dbind
* mvbind
* mvcall
* mvlist
* mvsetq
* mvprog1

# Deleted Symbols
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

# Deprecated Symbols
* remove-if-not (Use `keep-if`)
* delete-if-not (Use `nkeep-if`)
* nconc (Use `nappend`)

# Redefined
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

