# Differences with Common Lisp

## Naming of constants

All constant variables were renamed to the name added "+" signs before and after.

```common-lisp
+pi+
;=> 3.141592653589793d0

+array-rank-limit+
;=> 65529
```

## New data types

New data types were added.

* proper-list
* plist
* alist
* octet
* file-associated-stream
* character-designator
* function-designator
* file-position-designator
* list-designator
* package-designator
* stream-designator
* string-designator

Most of these are imported from [trivial-types](https://github.com/m2ym/trivial-types).

## String

A double quote character is a macro character which represents a string.

```common-lisp
"Hello, World!"
;=> "Hello, World!"
```

A backslash followed by some characters will be treated special.

```common-lisp
"Hello\nWorld!"
;=> "Hello
;   World!"
```

`#"` is similar to `"`, but it allows interpolation within strings.

If `${...}` or `@{...}` is seen, it will be replaced by the result value of an expression (or the last expression) inside of it.

```common-lisp
#"1 + 1 = ${(+ 1 1)}"
;=> "1 + 1 = 2"
```

## Hash table

CL21 provides a notation for hash-tables.

```common-lisp
#H(:name "Eitaro Fukamachi" :living "Japan")
;=> #H(:LIVING "Japan" :NAME "Eitaro Fukamachi")
```

Note this always creates a hash-table whose test function is `EQUAL`. If you want to create it with another test function, a function `hash-table` is also available.

```common-lisp
(hash-table 'eq :name "Eitaro Fukamachi")
;=> #H(:NAME "Eitaro Fukamachi")
```

## Vector

`#(...)` is a notation for vectors. Unlike in Common Lisp, its elements will be evaluated and it creates an adjustable vector.

```common-lisp
(let ((a 1)
      (b 2)
      (c 3))
  #(a b c))
;=> #(1 2 3)
```

```
(defvar *vec* #(0))

(push 1 *vec*)
;=> #(1 0)

(push-back 3 *vec*)
;=> #(1 0 3)

(pop *vec*)
;=> 1
```

## Lambda

`lm` is a macro for creating an anonymous function.

```common-lisp
(lm (x) (typep x 'cons))
;=> #<FUNCTION (LAMBDA (X)) {1008F50DAB}>

(map (lm (x) (+ 2 x)) '(1 2 3))
;=> (3 4 5)
```

`^` is a reader macro which will be expanded to `lm`.

```common-lisp
^(typep % 'cons)
;=> #<FUNCTION (LAMBDA (%1 &REST #:G1156 &AUX ...)) {10092490CB}>

(map ^(+ 2 %) '(1 2 3))
;=> (3 4 5)
```

Unused arguments will be ignored automatically.

```common-lisp
(map ^(random 10) (iota 10))
;=> (6 9 5 1 3 5 4 0 7 4)
```

`%n` designates the nth argument (1-based). `%` is a synonym for `%1`.

```common-lisp
(sort '(6 9 5 1 3 5 4 0 7 4) ^(< %1 %2))
;=> (0 1 3 4 4 5 5 6 7 9)
```

## Function

`function` is a special operator for getting a function value from a given form.

If a symbol is given, `function` returns a function value of it.

```common-lisp
(function integerp)
;=> #<FUNCTION INTEGERP>
```

If a form which starts with `compose`, `and`, `or` or `not`, `function` returns a composed function.

```common-lisp
(function (compose - *))
<=> (compose (function -) (function *))

(function (and integerp evenp))
<=> (conjoin (function integerp) (function evenp))

(function (or oddp zerop))
<=> (disjoin (function oddp) (function zerop))

(function (not zerop))
<=> (complement (function zerop))
```

`#'` is a reader macro for `function`.

```common-lisp
#'(compose - *)
#'(and integerp evenp)
#'(or oddp zerop)
#'(not zerop)
#'(and integerp (or oddp zerop))
```

## Builtin generic functions

There are several generic functions which have the same name to CL's normal functions.

* getf
* equalp
* emptyp
* coerce

```common-lisp
(defvar *hash* #H(:name "Eitaro Fukamachi" :living "Japan"))

(getf *hash* :name)
;=> "Eitaro Fukamachi"

(coerce *hash* 'plist)
;=> (:LIVING "Japan" :NAME "Eitaro Fukamachi")
```

You can define these methods for your own classes.

## Iteration

Common Lisp has simple iteration facilities: `dolist`, `dotimes` and `dolist`.

In addition, CL21 provides another one: `doeach`.

`doeach` is similar to `dolist`, but it can be used with any kind of sequences and hash-table.

```common-lisp
(doeach (x '("al" "bob" "joe"))
  (when (> (length x) 2)
    (princ #"${x}\n")))
;-> bob
;   joe

(doeach ((key value) #H('a 2 'b 3))
  (when (> value 2)
    (print key)))
;=> B
```

Destructuring binding form can be placed at the variable position.

```common-lisp
(doeach ((x y) '((1 2) (2 3) (3 4)))
  (print (+ x y)))
;-> 3
;   5
;   7
```

## Mapping

In Common Lisp, functions which have a name starts with "map" are higher-order functions take a function and a sequence.

It is same to CL21, but in CL21, "map" functions always return a value. This aims to clarify the roles of "iteration" and "mapping".

For that reason, CL21 doesn't have CL's `mapc` and `mapl`. `maphash` exists, but it returns a new hash table.

```common-lisp
(maphash (lm (k v)
           (cons k (1+ v)))
         #H(:a 1 :b 2))
;=> #H(:B 3 :A 2)
```

`map` is similar to Common Lisp's `mapcar` except it can take any kind of sequences, not only list.

```common-lisp
(map #'- '(1 2 3 4))
;=> (-1 -2 -3 -4)

(map #'- #(1 2 3 4))
;=> #(-1 -2 -3 -4)
```

CL21 doesn't have `mapcar`. Use `map` for instead.

## Abstract classes

CL21 provides a way to define a data structure which is close to the language.

"Abstract sequence" is a sequence like CLOS class which can be used with built-in sequence functions. It is similar to [User-extensible sequences](http://www.doc.gold.ac.uk/~mas01cr/papers/ilc2007/sequences-20070301.pdf) by Christophe Rhodes, but there are some differences in APIs.

"Abstract hash table" is a hash-table like CLOS class which can be used with built-in hash table functions.

See [documentation of abstract classes](https://github.com/fukamachi/cl21/blob/master/doc/Abstract_Classes.markdown) for the detail.

## Readtable for each packages

Managing readtables is annoying. We have to take care which readtable is enabled. Named-readtables makes it a bit easier, but it is still a troublesome thing.

In CL21, every package has own readtables. Your reader macros won't violate other libraries. It's safe.

## Syntax

"Syntax" is a set of reader macros which can be imported into readtables.

```common-lisp
;; An example from cl21.re

(defsyntax cl21.re
  ((#\# #\/) #'regexp-reader))

(export-syntax 'cl21.re)
```

Syntaxes can be exported by `export-syntax`. When `use-package` the package, exported syntaxes will be imported implicitly.

```common-lisp
(#/^Hello, (.+?)!$/ "Hello, World!")
;=> Error: no dispatch function defined for #\/

(defpackage myapp
  (:use :cl21
        :cl21.re))
(in-package :myapp)

(#/^Hello, (.+?)!$/ "Hello, World!")
;=> "Hello, World!"
;   #("World")
```

## CL21 Standard Library

CL21 Standard Library is a set of libraries that are distributed with CL21. It is intended to offer a wide range of facilities.

We are working on increasing the number of it. Currently, the following packages are provided.

* cl21.re
* cl21.process
* cl21.os
* cl21.lazy
* cl21.abbr
