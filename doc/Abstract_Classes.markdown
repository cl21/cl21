# Abstract Classes

## abstract-sequence

`abstract-sequence` is a vector-like abstraction class. You can define your own class which behaves like a built-in sequence by inheriting this class.

### Example

```common-lisp
(defclass my-vector (abstract-sequence)
  ((elements :type vector :initarg :elements :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defmethod make-sequence-like ((sequence my-vector) length &rest args)
  (make-instance 'my-vector :elements (apply #'make-sequence-like #() length args)))

(defmethod adjust-sequence ((vec my-vector) length &rest args &key initial-element initial-contents)
  (declare (ignore initial-element initial-contents))
  (setf (slot-value vec 'elements)
        (apply #'adjust-sequence (slot-value vec 'elements) length args))
  vec)

(defmethod abstract-elt ((vec my-vector) index)
  (elt (slot-value vec 'elements) index))

(defmethod (setf abstract-elt) (newval (vec my-vector) index)
  (setf (elt (slot-value vec 'elements) index) newval))

(defmethod abstract-length ((vec my-vector))
  (length (slot-value vec 'elements)))

;; just for usability
(defmethod print-object ((object my-vector) stream)
  (format stream "#<MY-VECTOR ~S>" (coerce object 'vector)))


;;
;; Usage

(defvar vec (make-instance 'my-vector :elements #(1 2 3)))

(take 2 vec)
;=> #<MY-VECTOR #(1 2)>
(keep-if #'evenp vec)
;=> #<MY-VECTOR #(2)>
```

### Requirements

Define a standard-class which inherits `abstract-sequence`. The class must have the following specialized methods.

* abstract-length
* abstract-elt
* (setf abstract-elt)
* make-sequence-like
* adjust-sequence

And, you can also define the following methods as you needed.

* emptyp
* make-sequence-iterator
* abstract-first
* (setf abstract-first)
* abstract-second
* (setf abstract-second)
* abstract-third
* (setf abstract-third)
* abstract-fourth
* (setf abstract-fourth)
* abstract-fifth
* (setf abstract-fifth)
* abstract-sixth
* (setf abstract-sixth)
* abstract-seventh
* (setf abstract-seventh)
* abstract-eighth
* (setf abstract-eighth)
* abstract-ninth
* (setf abstract-ninth)
* abstract-tenth
* (setf abstract-tenth)
* abstract-rest
* (setf abstract-rest)
* abstract-subseq
* (setf abstract-subseq)
* abstract-take
* abstract-drop
* abstract-take-while
* abstract-drop-while
* abstract-last
* abstract-butlast
* abstract-nbutlast
* abstract-find-if
* abstract-find-if
* abstract-position-if
* abstract-position
* abstract-search
* abstract-remove-if
* abstract-remove
* abstract-delete-if
* abstract-delete
* abstract-partition-if
* abstract-subdivide
* abstract-mismatch
* abstract-count-if
* abstract-count
* abstract-reverse
* abstract-nreverse
* abstract-reduce
* abstract-sort
* abstract-stable-sort
* abstract-substitute-if
* abstract-substitute
* abstract-nsubstitute-if
* abstract-nsubstitute
* abstract-remove-duplicates
* abstract-delete-duplicates
* abstract-split
* abstract-split-if
* abstract-copy-seq
* abstract-replace
* abstract-fill

### Limitations

Currently, `abstract-sequence` classes cannot be applied to `loop`, `doeach` and `destructuring-bind`, `merge` and `concatenate`.

## abstract-list

`abstract-list` is a subclass of `abstract-sequence`. This class is intended to be a list-like abstraction class. The difference between `abstract-sequence` and `abstract-list` is the way for iterations. `abstract-sequence` is good at random access. On the other hand, `abstract-list` is good at _sequential_ accesss.

### Example

cl21.lazy is an actual example.

```common-lisp
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

### Requirements

Define a standard-class which inherits `abstract-list`. The class must have the following specialized methods.

* emptyp
* abstract-first
* (setf abstract-first)
* abstract-rest
* (setf abstract-rest)
* make-sequence-like

`abstract-rest` must return the exact same object all the time.

And, you can also define the following methods as you needed.

* make-sequence-iterator
* abstract-length
* abstract-elt
* (setf abstract-elt)
* abstract-second
* (setf abstract-second)
* abstract-third
* (setf abstract-third)
* abstract-fourth
* (setf abstract-fourth)
* abstract-fifth
* (setf abstract-fifth)
* abstract-sixth
* (setf abstract-sixth)
* abstract-seventh
* (setf abstract-seventh)
* abstract-eighth
* (setf abstract-eighth)
* abstract-ninth
* (setf abstract-ninth)
* abstract-tenth
* (setf abstract-tenth)
* adjust-sequence
* abstract-subseq
* (setf abstract-subseq)
* abstract-take
* abstract-drop
* abstract-take-while
* abstract-drop-while
* abstract-last
* abstract-butlast
* abstract-nbutlast
* abstract-find-if
* abstract-find-if
* abstract-position-if
* abstract-position
* abstract-search
* abstract-remove-if
* abstract-remove
* abstract-delete-if
* abstract-delete
* abstract-partition-if
* abstract-subdivide
* abstract-mismatch
* abstract-count-if
* abstract-count
* abstract-reverse
* abstract-nreverse
* abstract-reduce
* abstract-sort
* abstract-stable-sort
* abstract-substitute-if
* abstract-substitute
* abstract-nsubstitute-if
* abstract-nsubstitute
* abstract-remove-duplicates
* abstract-delete-duplicates
* abstract-split
* abstract-split-if
* abstract-copy-seq
* abstract-replace
* abstract-fill
* abstract-member
* abstract-member-if
* abstract-flatten
* abstract-last-cons

### Limitations

Currently, `abstract-list` classes cannot be applied to `loop`, `doeach`, `destructuring-bind`, `merge`, `concatenate` and functions for normal lists, such as `car`, `cdr`, `cadr`, `nth`, `nthcdr`, `copy-list`, etc.

## abstract-hash-table

`abstract-hash-table` is a hash-table-like abstraction class.

### Requirements

Define a standard-class which inherits `abstract-hash-table`. The class must have the following specialized methods.

* abstract-gethash
* (setf abstract-gethash)

If you implement `make-sequence-iterator`, the class can be used in `map`, `some`, `every`, `notevery` and `notany`.

And, you can also define the following methods as you needed.

* abstract-hash-table-count
* abstract-hash-table-rehash-size
* abstract-hash-table-size
* abstract-hash-table-test
* abstract-remhash
* abstract-clrhash
* abstract-copy-hash-table
* emptyp
* abstract-hash-table-keys
* abstract-hash-table-values

### Limitations

Currently, `abstract-hash-table` classes cannot be applied to `with-hash-table-iterator`, `loop` and `doeach`.
