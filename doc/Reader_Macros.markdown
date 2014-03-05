# Reader Macros of CL21

## "..."

CL21 has a different string literal from Common Lisp. The only difference is that it allows C-like escape characters, such like `\n`.

```common-lisp
(princ "Hello, World!\n")
;-> Hello, World!
;=> "Hello, World!
;   "
```

## #"..."

`#"..."` is similar to `"..."`, but this allows string interpolation.

```common-lisp
(let ((name "John"))
  (princ #"Hello, ${name}!\n"))
;-> Hello, John!
;=> "Hello, John!
;   "
```

## #'

CL21's `#'` has some additional features comparing to Common Lisp's. You can use `and`, `or`, `not` and `compose` at the first element.

```common-lisp
(keep-if #'(and integerp evenp) '(1 2 3 2.0 4))
;=> (2 4)

(keep-if #'(and integerp
                (or (not evenp)
                    zerop))
           (iota 11))
;=> (0 1 3 5 7 9)
```

This reader is same as `(function ...)`.

## ^

This is a shorthand for `lambda`.

```common-lisp
^(coerce _ 'list)   <=> (lambda (x) (coerce x 'list))
^(* (sin (1+ _)) _) <=> (lambda (x y) (* (sin (1+ x)) y))
^(apply #'list _ (length _) _..)
  <=> (lambda (x y &rest z) (apply #'list x (length y) z))
```

The specification isn't fixed yet. The discussion is still going at [#35](https://github.com/fukamachi/cl21/issues/35).

## #(...)

`#(...)` is a reader macro for making a simple-vector. The differences from Common Lisp's are these.

* Evaluates all elements.
* Make an adjustable vector.

```common-lisp
(defvar *vector* #())

(push 1 *vector*)
(elt *vector* 0)
;=> 1

(push 3 *vector*)
(elt *vector* 0)
;=> 3

(pop *vector*)
;=> 3
(pop *vector*)
;=> 1
```

## #H(...)

`#H(...)` is a reader macro for making a hash-table whose test function is `equal`.

```common-lisp
#H(:a 1 :b 2 :c 3)
;=> #H(:C 3 :B 2 :A 1)

(getf #H(:a 1 :b 2 :c 3) :b)
;=> 2
;   T
```
