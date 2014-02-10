(in-package :cl-user)
(defpackage cl21.core.sequence
  (:use :cl)
  (:shadow :push
           :pushnew
           :pop
           :last
           :butlast
           :nbutlast
           :first
           :second
           :third
           :fourth
           :fifth
           :sixth
           :seventh
           :eighth
           :ninth
           :tenth
           :rest
           :subseq
           :find
           :find-if
           :position
           :position-if
           :search
           :elt
           :remove
           :remove-if
           :remove-if-not
           :delete
           :delete-if
           :delete-if-not
           :mismatch
           :length
           :count
           :count-if
           :map
           :map-into
           :reverse
           :nreverse
           :reduce
           :sort
           :stable-sort
           :substitute
           :substitute-if
           :nsubstitute
           :nsubstitute-if
           :member
           :member-if
           :remove-duplicates
           :delete-duplicates)
  (:shadowing-import-from :cl21.core.generic
                          :emptyp
                          :coerce)
  (:import-from :cl21.core.cltl2
                :variable-information)
  (:import-from :cl21.core.util
                :define-typecase-compiler-macro)
  #+nil
  (:import-from :split-sequence
                :split-sequence
                :split-sequence-if)
  (:import-from :alexandria
                :delete-from-plist)
  (:export :split-sequence
           :split-sequence-if

           :sequence
           :fill
           :copy-seq
           :make-sequence
           :subseq
           :reduce
           :count
           :count-if
           :length
           :reverse
           :nreverse
           :sort
           :stable-sort

           :find
           :find-if
           :position
           :position-if

           :search
           :mismatch
           :replace
           :concatenate
           :elt
           :merge

           :map
           :map-into

           :member
           :member-if

           :remove
           :remove-if
           :remove-if-not
           :keep
           :keep-if
           :nkeep
           :nkeep-if
           :remove-duplicates

           :delete
           :delete-if
           :delete-if-not
           :delete-duplicates

           :substitute
           :substitute-if
           :nsubstitute
           :nsubstitute-if

           :push
           :pushnew
           :pop
           :last
           :butlast
           :nbutlast
           :first
           :second
           :third
           :fourth
           :fifth
           :sixth
           :seventh
           :eighth
           :ninth
           :tenth
           :rest

           ;; Alexandria
           :length=

           :take
           :drop
           :take-while
           :drop-while
           :partition
           :partition-if
           :subdivide
           :concat

           :abstract-sequence
           :abstract-list
           :abstract-vector
           :abstract-first
           :abstract-rest
           :abstract-cons
           :abstract-vector-push
           :abstract-vector-pop
           :abstract-elt
           :abstract-subseq
           :abstract-length
           :abstract-take
           :abstract-drop
           :abstract-take-while
           :abstract-drop-while
           :abstract-last
           :abstract-butlast
           :abstract-find-if
           :abstract-find
           :abstract-position-if
           :abstract-position
           :abstract-search
           :abstract-remove-if
           :abstract-remove
           :abstract-partition-if
           :abstract-subdivide
           :abstract-mismatch
           :abstract-count-if
           :abstract-count
           :abstract-reverse
           :abstract-reduce
           :abstract-sort
           :abstract-stable-sort
           :abstract-substitute-if
           :abstract-substitute
           :abstract-remove-duplicates
           :abstract-split-sequence
           :abstract-split-sequence-if
           :abstract-member
           :abstract-member-if))
(in-package :cl21.core.sequence)

#.`(progn
     ,@(loop for (function . n) in '((second . 1)
                                     (third . 2)
                                     (fourth . 3)
                                     (fifth . 4)
                                     (sixth . 5)
                                     (seventh . 6)
                                     (eighth . 7)
                                     (ninth . 8)
                                     (tenth . 9))
             collect `(defun ,function (seq)
                        (etypecase seq
                          (list (,(intern (string function) :cl) seq))
                          (cl:sequence (cl:elt seq ,n))))))


;;
;; Macro

(defmacro define-sequence-function-by-if (name name-of-if &optional (define-generic t))
  (let* ((cl-name (and (find-symbol (string name) :cl)
                       (intern (string name) :cl)))
         (abstract-name (intern (format nil "~A-~A" #.(string :abstract) (string name))))
         (apply-abstract-form `(apply (function ,abstract-name) item sequence args))
         (main-form `((when test-specified-p
                        (setq args (delete-from-plist args :test)))
                      (apply (function ,name-of-if)
                             (lambda (x) (funcall test x item))
                             sequence
                             args))))
    `(progn
       (defun ,name (item sequence &rest args &key from-end start end key (test #'eql test-specified-p))
         (declare (ignorable from-end start end key test test-specified-p))
         ,@(if cl-name
               `((typecase sequence
                   (sequence (apply (function ,cl-name) item sequence args))
                   (abstract-sequence ,@(if define-generic
                                            (list apply-abstract-form)
                                            main-form))))
               (if define-generic
                   (list apply-abstract-form)
                   main-form)))
       ,@(if define-generic
             `((defgeneric ,abstract-name
                   (item sequence &key from-end start end key test)
                 (:method (item (sequence abstract-sequence) &rest args &key from-end start end key (test #'eql test-specified-p))
                   (declare (ignore from-end start end key))
                   ,@main-form)))
             nil)
       ,@(if cl-name
             `((define-typecase-compiler-macro ,name (&whole form item sequence &rest args)
                 (typecase sequence
                   (sequence (list* 'apply (function ,cl-name) item sequence (cdddr form))))))
             nil))))


;;
;; Abstract Sequence

(defclass abstract-sequence () ())
(defclass abstract-list (abstract-sequence) ())
(defclass abstract-vector (abstract-sequence) ())

(defmethod coerce ((object abstract-sequence) type)
  (ecase type
    (list (loop with seq = object
                until (emptyp seq)
                collect (first seq)
                do (setf seq (rest seq))))
    ((vector simple-vector)
     (apply #'vector (coerce object 'list)))))

(defgeneric abstract-cons (object sequence)
  (:method (object (sequence abstract-list))
    (declare (ignore object))
    (error "Method ABSTRACT-CONS for ~A isn't implemented."
           (class-name (class-of sequence)))))

(defun concat (sequence &rest more-sequences)
  (let ((type (etypecase sequence
                (string 'string)
                (list   'list)
                (vector 'vector))))
    (apply #'concatenate
           type
           sequence
           more-sequences)))


;;
;; Macro: push, pushnew, pop
;; Generic Function: abstract-vector-push, abstract-vector-pop

(defmacro push (value place)
  (let ((place-g (gensym "PLACE")))
    `(let ((,place-g ,place))
       (etypecase ,place-g
         (list (cl:push ,value ,place))
         (vector (vector-push-extend ,value ,place-g))
         (abstract-vector (abstract-vector-push ,value ,place-g))
         (abstract-sequence
          ,(if (symbolp place)
               `(setf ,place (abstract-cons ,value ,place-g))
               `(cl:push ,value ,place)))))))
(defgeneric abstract-vector-push (new-element vector))

(defmacro pushnew (value place &rest keys &key key test test-not)
  (declare (ignore key test test-not))
  (let ((place-g (gensym "PLACE")))
    `(let ((,place-g ,place))
       (etypecase ,place-g
         (vector (or (cl:find ,value ,place-g ,@keys)
                     (vector-push-extend ,value ,place-g)))
         (list (cl:pushnew ,value ,place-g ,@keys))
         (abstract-sequence (or (find ,value ,place-g ,@keys)
                                (push ,value ,place-g)))))))

(defmacro pop (place)
  (let ((place-g (gensym "PLACE")))
    `(let ((,place-g ,place))
       (etypecase ,place-g
         (vector (cl:vector-pop ,place-g))
         (list (cl:pop ,place))
         (abstract-vector (abstract-vector-pop ,place-g))
         (abstract-sequence
          (prog1
              (abstract-first ,place-g)
            ,(if (or (not (listp place))
                     (fboundp `(setf ,(car place))))
                 `(setf ,place (abstract-rest ,place-g))
                 nil)))))))
(defgeneric abstract-vector-pop (vector))


;;
;; Function: first, rest
;; Generic Function: abstract-first, abstract-rest

(defun first (seq)
  (etypecase seq
    (list (cl:first seq))
    (sequence (cl:elt seq 0))
    (abstract-sequence (abstract-first seq))))
(defgeneric abstract-first (seq)
  (:method ((seq abstract-list))
    (error "Method ABSTRACT-FIRST for ~A isn't implemented."
           (class-name (class-of seq))))
  (:method ((seq abstract-vector))
    (abstract-elt seq 0)))

(defun rest (seq)
  (etypecase seq
    (list (cl:rest seq))
    (sequence (drop 1 seq))
    (abstract-sequence (abstract-rest seq))))
(defgeneric abstract-rest (seq)
  (:method ((seq abstract-list))
    (error "Method ABSTRACT-REST for ~A isn't implemented."
           (class-name (class-of seq))))
  (:method ((seq abstract-vector))
    (abstract-subseq seq 1)))


;;
;; Function: elt
;; Generic Function: abstract-elt

(defun elt (sequence index)
  (etypecase sequence
    (sequence (cl:elt sequence index))
    (abstract-sequence (abstract-elt sequence index))))
(define-typecase-compiler-macro elt (sequence index)
  (typecase sequence
    (sequence `(cl:elt ,sequence ,index))))

(defgeneric abstract-elt (sequence index)
  (:method ((sequence abstract-list) index)
    (abstract-first (drop index sequence)))
  (:method ((sequence abstract-vector) index)
    (declare (ignore index))
    (error "Method ABSTRACT-ELT for ~A isn't implemented."
           (class-name (class-of sequence)))))

;;
;; Function: length
;; Generic Function: abstract-length

(defun length (sequence)
  (etypecase sequence
    (sequence (cl:length sequence))
    (abstract-sequence (abstract-length sequence))))
(define-typecase-compiler-macro length (sequence)
  (typecase sequence
    (sequence `(cl:length ,sequence))))

(defgeneric abstract-length (sequence)
  (:method ((sequence abstract-list))
    (cl:length (coerce sequence 'list)))
  (:method ((sequence abstract-vector))
    (cl:length (abstract-subseq sequence 0))))


;;
;; Function: subseq
;; Generic Function: abstract-subseq

(defun subseq (sequence start &optional end)
  (etypecase sequence
    (sequence (cl:subseq sequence start end))
    (abstract-sequence (abstract-subseq sequence start end))))
(define-typecase-compiler-macro subseq (&whole form sequence start &optional end)
  (typecase sequence
    (sequence `(cl:subseq ,@(cdr form)))))

(defgeneric abstract-subseq (sequence start &optional end)
  (:method ((sequence abstract-list) start &optional end)
    (if end
        (take (- end start) (drop start sequence))
        (coerce (drop start sequence) 'list)))
  (:method ((sequence abstract-vector) start &optional end)
    (declare (ignore start end))
    (error "Method ABSTRACT-SUBSEQ for ~A isn't implemented."
           (class-name (class-of sequence)))))


;;
;; Function: take, drop
;; Generic Function: abstract-take, abstract-drop

(defun take (n sequence)
  (etypecase sequence
    (sequence (cl:subseq sequence 0 n))
    (abstract-sequence (abstract-take n sequence))))
(define-typecase-compiler-macro take (n sequence)
  (typecase sequence
    (sequence `(cl:subseq ,sequence 0 ,n))))

(defgeneric abstract-take (n sequence)
  (:method (n (sequence abstract-vector))
    (abstract-subseq sequence 0 n))
  (:method (n (sequence abstract-list))
    (if (zerop n)
        nil
        (cons (abstract-first sequence)
              (take (1- n) (abstract-rest sequence))))))

(defun drop (n sequence)
  (etypecase sequence
    (list (nthcdr n sequence))
    (sequence (cl:subseq sequence n))
    (abstract-sequence (abstract-drop n sequence))))
(define-typecase-compiler-macro drop (n sequence)
  (typecase sequence
    (list `(nthcdr ,n ,sequence))
    (sequence `(cl:subseq ,sequence ,n))))

(defgeneric abstract-drop (n sequence)
  (:method (n (sequence abstract-vector))
    (abstract-subseq sequence n))
  (:method (n (sequence abstract-list))
    (if (zerop n)
        sequence
        (drop (1- n) (abstract-rest sequence)))))


;;
;; Function: take-while, drop-while
;; Generic Function: abstract-take-while, abstract-drop-while

(defun %sequence-take-while (pred sequence)
  (let ((pos (cl:position-if (complement pred) sequence)))
    (if pos
        (cl:subseq sequence 0 pos)
        sequence)))

(defun take-while (pred sequence)
  (etypecase sequence
    (sequence (%sequence-take-while pred sequence))
    (abstract-sequence (abstract-take-while pred sequence))))
(define-typecase-compiler-macro take-while (pred sequence)
  (typecase sequence
    (sequence `(%sequence-take-while ,pred ,sequence))))

(defgeneric abstract-take-while (pred sequence)
  (:method (pred (sequence abstract-vector))
    (loop for i from 0 to (1- (abstract-length sequence))
          for x = (abstract-elt sequence i)
          while (funcall pred x)
          finally
             (return (abstract-subseq sequence 0 (1- i)))))
  (:method (pred (sequence abstract-list))
    (loop with seq = sequence
          until (emptyp seq)
          for x = (pop seq)
          while (funcall pred x)
          collect x)))

(defun %sequence-drop-while (pred sequence)
  (let ((pos (cl:position-if (complement pred) sequence)))
    (if pos
        (cl:subseq sequence pos)
        (cl:subseq sequence 0 0))))

(defun drop-while (pred sequence)
  (etypecase sequence
    (sequence (%sequence-drop-while pred sequence))
    (abstract-sequence (abstract-drop-while pred sequence))))
(define-typecase-compiler-macro drop-while (pred sequence)
  (typecase sequence
    (sequence `(%sequence-drop-while ,pred ,sequence))))

(defgeneric abstract-drop-while (pred sequence)
  (:method (pred (sequence abstract-vector))
    (loop for i from 0 to (1- (abstract-length sequence))
          for x = (abstract-elt sequence i)
          while (funcall pred x)
          finally
             (return (abstract-subseq sequence i))))
  (:method (pred (sequence abstract-list))
    (loop with seq = sequence
          until (emptyp seq)
          while (funcall pred (abstract-first seq))
          do (setf seq (abstract-rest seq))
          finally (return seq))))


;;
;; Function: last
;; Generic Function: abstract-last

(defun last (sequence)
  (etypecase sequence
    (list (car (cl:last sequence)))
    (sequence (cl:elt sequence (1- (cl:length sequence))))
    (abstract-sequence (abstract-last sequence))))
(define-typecase-compiler-macro last (sequence)
  (typecase sequence
    (list `(car (cl:last ,sequence)))
    (sequence `(cl:elt ,sequence (1- (cl:length ,sequence))))))

(defgeneric abstract-last (sequence)
  (:method ((sequence abstract-vector))
    (abstract-elt sequence (1- (abstract-length sequence))))
  (:method ((sequence abstract-list))
    (labels ((until-last (seq)
               (let ((next (and seq
                                (abstract-rest seq))))
                 (if (emptyp next)
                     (abstract-first seq)
                     (until-last (abstract-rest seq))))))
      (until-last sequence))))


;;
;; Function: butlast, nbutlast
;; Generic Function: abstract-butlast

(defun butlast (sequence &optional (n 1))
  (assert (>= n 0))
  (etypecase sequence
    (list (cl:butlast sequence n))
    (sequence (cl:subseq sequence 0 (- (cl:length sequence) n)))
    (abstract-sequence (abstract-butlast sequence n))))
(define-typecase-compiler-macro butlast (sequence &optional n)
  (typecase sequence
    (list `(cl:butlast ,sequence ,(or n 1)))
    (sequence `(cl:subseq ,sequence 0 (- (cl:length ,sequence) ,(or n 1))))))

(defgeneric abstract-butlast (sequence &optional n)
  (:method ((sequence abstract-vector) &optional (n 1))
    (abstract-subseq sequence 0 (- (abstract-length sequence) n)))
  (:method ((sequence abstract-list) &optional (n 1))
    (butlast (coerce sequence 'list) n)))

(defun nbutlast (sequence &optional (n 1))
  (etypecase sequence
    (list (cl:nbutlast sequence n))
    (abstract-sequence (butlast sequence n))))
(define-typecase-compiler-macro nbutlast (sequence &optional n)
  (typecase sequence
    (list `(cl:nbutlast ,sequence ,(or n 1)))))


;;
;; Function: find, find-if
;; Generic Function: abstract-find, abstract-find-if

(defun find-if (pred sequence &rest args &key from-end start end key)
  (declare (ignore from-end start end key))
  (etypecase sequence
    (sequence (apply #'cl:find-if pred sequence args))
    (abstract-sequence (apply #'abstract-find-if pred sequence args))))
(define-typecase-compiler-macro find-if (&whole form pred sequence &rest args)
  (typecase sequence
    (sequence `(cl:find-if ,@(cdr form)))))

(defgeneric abstract-find-if (pred sequence &key from-end start end key)
  (:method (pred (sequence abstract-vector) &rest args &key from-end (start 0) end (key #'identity))
    (if from-end
        (apply #'cl:find-if pred (coerce sequence 'vector) args)
        (loop for i from start to (or end
                                      (1- (abstract-length sequence)))
              for x = (abstract-elt sequence i)
              when (funcall pred (funcall key x))
                return x)))
  (:method (pred (sequence abstract-list) &rest args &key from-end (start 0) end (key #'identity))
    (if from-end
        (apply #'cl:find-if pred (coerce sequence 'list) args)
        (loop with seq = (drop start sequence)
              with i = 0
              until (or (and end
                             (<= end i))
                        (emptyp seq))
              for x = (pop seq)
              when (funcall pred (funcall key x))
                return x
              do (incf i)))))

(define-sequence-function-by-if find find-if)


;;
;; Function: position, position-if
;; Generic Function: abstract-position, abstract-position-if

(defun position-if (pred sequence &rest args &key from-end start end key)
  (declare (ignore from-end start end key))
  (etypecase sequence
    (sequence (apply #'cl:position-if pred sequence args))
    (abstract-sequence (apply #'abstract-position-if pred sequence args))))
(define-typecase-compiler-macro position-if (&whole form pred sequence &rest args)
  (typecase sequence
    (sequence `(apply #'cl:position-if ,pred ,sequence ,@(cdddr form)))))

(defgeneric abstract-position-if (pred sequence &key from-end start end key)
  (:method (pred (sequence abstract-vector) &rest args &key from-end (start 0) end (key #'identity))
    (if from-end
        (apply #'cl:position-if pred (coerce sequence 'vector) args)
        (loop for i from start to (or end
                                      (1- (abstract-length sequence)))
              for x = (abstract-elt sequence i)
              when (funcall pred (funcall key x))
                return i)))
  (:method (pred (sequence abstract-list) &rest args &key from-end (start 0) end (key #'identity))
    (if from-end
        (apply #'cl:position-if pred (coerce sequence 'list) args)
        (loop with seq = (drop start sequence)
              with i = 0
              for x = (pop seq)
              until (or (and end
                             (<= end i))
                        (emptyp seq))
              when (funcall pred (funcall key x))
                return (+ i start)
              do (incf i)))))

(define-sequence-function-by-if position position-if)


;;
;; Function: search
;; Generic Function: abstract-search

(defun search (sequence1 sequence2 &rest args &key from-end test start1 end1 start2 end2 key)
  (declare (ignore from-end test start1 end1 start2 end2 key))
  (etypecase sequence2
    (sequence (apply #'cl:search sequence1 sequence2 args))
    (abstract-sequence (apply #'abstract-search sequence1 sequence2 args))))
(define-typecase-compiler-macro search (&whole form sequence1 sequence2 &rest args)
  (typecase sequence2
    (sequence `(cl:search ,@(cdr form)))))

(defgeneric abstract-search (sequence1 sequence2 &key from-end test start1 end1 start2 end2 key)
  (:method (sequence1 (sequence2 abstract-vector) &rest args &key from-end (test #'eql) (start1 0) end1 (start2 0) end2 (key #'identity))
    (when (typep sequence1 'abstract-sequence)
      (setq sequence1 (coerce sequence1 'vector)))
    (if from-end
        (apply #'cl:search sequence1 (coerce sequence2 'vector) args)
        (progn
          (setq sequence1 (cl:subseq sequence1 start1 end1))
          (loop with first-el = (funcall key (elt sequence1 0))
                with len = (cl:length sequence1)
                for i from start2 to (or end2
                                         (1- (abstract-length sequence2)))
                for x = (abstract-elt sequence2 i)
                when (and (funcall test (funcall key x) first-el)
                          (every (lambda (x y)
                                   (funcall test (funcall key x) (funcall key y)))
                                 sequence1
                                 (subseq sequence2 i (+ i len))))
                  return i))))
  (:method (sequence1 (sequence2 abstract-list) &rest args &key from-end (test #'eql) (start1 0) end1 (start2 0) end2 (key #'identity))
    (when (typep sequence1 'abstract-sequence)
      (setq sequence1 (coerce sequence1 'list)))
    (if from-end
        (apply #'cl:search sequence1 (coerce sequence2 'list) args)
        (progn
          (setq sequence1 (subseq sequence1 start1 end1))
          (loop with first-el = (funcall key (elt sequence1 0))
                with len = (cl:length sequence1)
                with seq = (drop start2 sequence2)
                with i = 0
                until (or (and end2
                               (< end2 (+ i len)))
                          (emptyp seq))
                for x = (pop seq)
                when (and (funcall test (funcall key x) first-el)
                          (every (lambda (x y)
                                   (funcall test (funcall key x) (funcall key y)))
                                 (drop 1 sequence1)
                                 (take (1- len) seq)))
                  return (+ i start2)
                do (incf i))))))


;;
;; Function: remove, remove-if, remove-if-not, delete, delete-if, delete-if-not, keep, keep-if, nkeep, nkeep-if
;; Generic Function: abstract-remove, abstract-remove-if

(defun remove-if (pred sequence &rest args &key from-end start end count key)
  (declare (ignore from-end start end count key))
  (etypecase sequence
    (sequence (apply #'cl:remove-if pred sequence args))
    (abstract-sequence (apply #'abstract-remove-if pred sequence args))))

;; NOTE: This generic function affects other functions: remove-if-not, delete-if, delete-if-not, keep-if, nkeep-if
(defgeneric abstract-remove-if (pred sequence &key from-end start end count key)
  (:method (pred (sequence abstract-vector) &rest args &key from-end (start 0) end count (key #'identity))
    (if from-end
        (apply #'cl:remove-if pred (coerce sequence 'vector) args)
        (loop for i from start to (or end
                                      (1- (abstract-length sequence)))
              for x = (abstract-elt sequence i)
              until (and count
                         (zerop count))
              when (funcall pred (funcall key x))
                do (and count (decf count))
              else
                collect x into buf
              finally (return
                        (concatenate 'vector
                                     buf
                                     ;; FIXME: No need to coerce.
                                     (coerce (drop i sequence) 'vector))))))
  (:method (pred (sequence abstract-list) &rest args &key from-end (start 0) end count (key #'identity))
    (if (or (not count)
            end
            from-end)
        (apply #'cl:remove-if pred (coerce sequence 'list) args)
        (loop with seq = (drop start sequence)
              until (or (zerop count)
                        (emptyp seq))
              for x = (pop seq)
              when (funcall pred (funcall key x))
                do (decf count)
              else
                collect x into buf
              finally (return
                        ;; FIXME: No need to coerce.
                        (concatenate 'list buf (coerce seq 'list)))))))
(define-typecase-compiler-macro remove-if (&whole form pred sequence &rest args)
  (typecase sequence
    (sequence `(cl:remove-if ,@(cdr form)))))

(define-sequence-function-by-if remove remove-if)

(defun remove-if-not (pred sequence &rest args &key from-end start end count key)
  (declare (ignore from-end start end count key))
  (etypecase sequence
    (sequence (apply #'cl:remove-if-not pred sequence args))
    (abstract-sequence
     (apply #'remove-if (complement pred) sequence args))))
(define-typecase-compiler-macro remove-if-not (&whole form pred sequence &rest args)
  (typecase sequence
    (sequence `(cl:remove-if-not ,@(cdr form)))))

(defun delete-if (pred sequence &rest args &key from-end (start 0) end count (key #'identity))
  (declare (ignore from-end start end count key))
  (etypecase sequence
    (sequence (apply #'cl:delete-if pred sequence args))
    (abstract-sequence
     (apply #'remove-if pred sequence args))))
(define-typecase-compiler-macro delete-if (&whole form pred sequence &rest args)
  (typecase sequence
    (sequence `(cl:delete-if ,@(cdr form)))))

(define-sequence-function-by-if delete delete-if nil)

(defun delete-if-not (pred sequence &rest args &key from-end (start 0) end count (key #'identity))
  (declare (ignore from-end start end count key))
  (etypecase sequence
    (sequence (apply #'cl:delete-if-not args))
    (abstract-sequence
     (apply #'delete-if (complement pred) sequence args))))
(define-typecase-compiler-macro delete-if-not (&whole form pred sequence &rest args)
  (typecase sequence
    (sequence `(cl:delete-if-not ,@(cdr form)))))

(setf (symbol-function 'keep-if) #'cl:remove-if-not)
(define-typecase-compiler-macro keep-if (&whole form pred sequence &rest args)
  (typecase sequence
    (sequence `(cl:remove-if-not ,@(cdr form)))))

(setf (symbol-function 'nkeep-if) #'cl:delete-if-not)
(define-typecase-compiler-macro nkeep-if (&whole form pred sequence &rest args)
  (typecase sequence
    (sequence `(cl:delete-if-not ,@(cdr form)))))

(define-sequence-function-by-if keep keep-if nil)

(define-sequence-function-by-if nkeep nkeep-if nil)

(defun %partition-if (pred sequence &key from-end (start 0) end (key #'identity))
  (let ((yes nil)
        (no nil)
        (sequence (subseq sequence start end)))
    (cl:map nil
            #'(lambda (x)
                (if (funcall pred (funcall key x))
                    (cl:push x yes)
                    (cl:push x no)))
            (if from-end
                (cl:nreverse sequence)
                sequence))
    (values yes no)))

(defun partition-if (pred sequence &rest args &key from-end start end key)
  "Given a predicate PRED, partition SEQUENCE into two sublists, the first
of which has elements that satisfy PRED, the second which do not."
  (declare (ignore from-end start end key))
  (etypecase sequence
    (sequence (apply #'%partition-if pred sequence args))
    (abstract-sequence (apply #'abstract-partition-if pred sequence args))))

(defgeneric abstract-partition-if (pred sequence &key from-end start end key)
  (:method (pred (sequence abstract-sequence) &rest args &key from-end start end key)
    (declare (ignore from-end start end key))
    (apply #'%partition-if pred sequence args)))

(define-sequence-function-by-if partition partition-if)


;;
;; Function: subdivide
;; Generic Function: abstract-subdivide

(defun subdivide (sequence chunk-size)
  "Split `sequence` into subsequences of size `chunk-size`."
  (check-type chunk-size (integer 1))

  (etypecase sequence
    ; Since lists have O(N) access time, we iterate through manually,
    ; collecting each chunk as we pass through it. Using SUBSEQ would
    ; be O(N^2).
    (list (loop :while sequence
                :collect
                (loop :repeat chunk-size
                      :while sequence
                      :collect (pop sequence))))

    ; For other sequences like strings or arrays, we can simply chunk
    ; by repeated SUBSEQs.
    (cl:sequence (loop :with len := (cl:length sequence)
                       :for i :below len :by chunk-size
                       :collect (cl:subseq sequence i (min len (+ chunk-size i)))))
    (abstract-sequence (abstract-subdivide sequence chunk-size) )))

(defgeneric abstract-subdivide (sequence chunk-size)
  (:method ((sequence abstract-vector) chunk-size)
    (subdivide (coerce sequence 'vector) chunk-size))
  (:method ((sequence abstract-list) chunk-size)
    (subdivide (coerce sequence 'list) chunk-size)))


;;
;; Function: mismatch
;; Generic Function: abstract-mismatch

(defun mismatch (sequence1 sequence2 &rest args &key from-end test start1 end1 start2 end2 key)
  (declare (ignore from-end test start1 end1 start2 end2 key))
  (etypecase sequence2
    (sequence (apply #'cl:mismatch sequence1 sequence2 args))
    (abstract-sequence (apply #'abstract-mismatch sequence1 sequence2 args))))
(define-typecase-compiler-macro mismatch (&whole form sequence1 sequence2 &rest args)
  (typecase sequence2
    (sequence `(cl:mismatch ,@(cdr form)))))

(defgeneric abstract-mismatch (sequence1 sequence2 &key from-end test start1 end1 start2 end2 key)
  (:method (sequence1 (sequence2 abstract-vector) &rest args &key from-end (test #'eql) (start1 0) end1 (start2 0) end2 (key #'identity))
    (when (typep sequence1 'abstract-sequence)
       (setq sequence1 (coerce sequence1 'vector)))
    (if from-end
        (apply #'cl:mismatch sequence1 (coerce sequence2 'vector) args)
        (cond
          ((and (null sequence1)
                (emptyp sequence2))
           nil)
          ((funcall test
                    (funcall key (abstract-elt sequence2 start2))
                    (funcall key (elt sequence1 start1)))
           (progn
             (setq sequence1 (cl:subseq sequence1 start1 end1))
             (mismatch (drop 1 sequence1)
                       (drop 1 sequence2)
                       :test test
                       :key key
                       :end1 (and end1 (1- end1))
                       :end2 (and end2 (1- end2)))))
          (T 0))))
  (:method (sequence1 (sequence2 abstract-list) &rest args &key from-end (test #'eql) (start1 0) end1 (start2 0) end2 (key #'identity))
    (when (typep sequence1 'abstract-sequence)
      (setq sequence1 (coerce sequence1 'list)))
    (if from-end
        (apply #'cl:mismatch sequence1 (coerce sequence2 'list) args)
        (if (funcall test
                     (funcall key (elt sequence2 start2))
                     (funcall key (elt sequence1 start1)))
            (progn
              (setq sequence1 (subseq sequence1 start1 end1))
              (loop with first-el = (funcall key (car sequence1))
                    with len = (cl:length sequence1)
                    with seq = (drop start2 sequence2)
                    with i = 0
                    until (or (and end2
                                   (< end2 (+ i len)))
                              (emptyp seq))
                    for x = (pop seq)
                    when (funcall test (funcall key x) first-el)
                      return (let ((res (mismatch (drop 1 sequence1) (take (1- len) seq)
                                                  :test test :key key)))
                               (if res
                                   (+ start2 res)
                                   nil))
                    do (incf i)))
            0))))


;;
;; Function: length=

(defun length= (&rest sequences)
  (if (every (lambda (seq) (not (typep seq 'abstract-sequence)))
             sequences)
      (apply #'alexandria:length= sequences)
      (multiple-value-bind (intargs sequences)
          (partition-if #'integerp sequences)
        (labels ((check-take (n seq)
                   (cond
                     ((zerop n) t)
                     ((emptyp seq) nil)
                     (T (check-take (1- n) (rest seq)))))
                 (check-seqs (sequences)
                   (if (every (lambda (seq) (check-take 1 seq)) sequences)
                       (check-seqs (mapcar #'rest sequences))
                       (every #'emptyp sequences))))
          (cond
            ((and (cdr intargs)
                  (not (apply #'= intargs)))
             nil)
            (intargs
             (if (zerop (car intargs))
                 (every #'emptyp sequences)
                 (not (some (lambda (seq) (check-take (1+ (car intargs)) seq))
                            sequences))))
            (T (check-seqs sequences)))))))
(define-compiler-macro length= (&whole form &environment env &rest sequences)
  (flet ((get-type (x)
           (if (constantp x)
               (type-of x)
               (and (symbolp x)
                    (assoc 'type
                           (nth-value 2 (variable-information x env)))))))
    (if (every (lambda (seq)
                 (let ((type (get-type seq)))
                   (and type
                        (subtypep type 'sequence))))
               sequences)
        `(alexandria:length= ,@(cdr form))
        form)))


;;
;; Function: count, count-if
;; Generic Function: abstract-count, abstract-count-if

(defun count-if (pred sequence &rest args &key from-end start end key)
  (declare (ignore from-end start end key))
  (etypecase sequence
    (sequence (apply #'cl:count-if pred sequence args))
    (abstract-sequence (apply #'abstract-count-if pred sequence args))))
(define-typecase-compiler-macro count-if (&whole form pred sequence &rest args)
  (typecase sequence
    (sequence `(cl:count-if ,@(cdr form)))))

(defgeneric abstract-count-if (pred sequence &key from-end start end key)
  (:method (pred (sequence abstract-vector) &rest args &key from-end start end key)
    (declare (ignore from-end start end key))
    (apply #'cl:count-if pred (coerce sequence 'vector) args))
  (:method (pred (sequence abstract-list) &rest args &key from-end start end key)
    (declare (ignore from-end start end key))
    (apply #'cl:count-if pred (coerce sequence 'list) args)))

(define-sequence-function-by-if count count-if)


;;
;; Function: map, map-into

(defun map (result-type function &rest sequences)
  (if (every (lambda (seq) (not (typep seq 'abstract-sequence)))
             sequences)
      (apply #'cl:map result-type function sequences)
      (apply #'cl:map result-type function (mapcar (lambda (seq)
                                                     (typecase seq
                                                       (abstract-vector (coerce seq 'vector))
                                                       (abstract-sequence (coerce seq 'list))
                                                       (otherwise seq)))
                                                   sequences))))
(define-compiler-macro map (&whole form &environment env result-type function &rest sequences)
  (declare (ignore result-type function))
  (flet ((get-type (x)
           (if (constantp x)
               (type-of x)
               (and (symbolp x)
                    (assoc 'type
                           (nth-value 2 (variable-information x env)))))))
    (if (every (lambda (seq)
                 (let ((type (get-type seq)))
                   (and type
                        (subtypep type 'sequence))))
               sequences)
        `(cl:map ,@(cdr form))
        form)))

(defun map-into (result-type function &rest sequences)
  (if (every (lambda (seq) (not (typep seq 'abstract-sequence)))
             sequences)
      (apply #'cl:map-into result-type function sequences)
      (apply #'cl:map-into result-type function (mapcar (lambda (seq)
                                                          (typecase seq
                                                            (abstract-vector (coerce seq 'vector))
                                                            (abstract-sequence (coerce seq 'list))
                                                            (otherwise seq)))
                                                        sequences))))
(define-compiler-macro map-into (&whole form &environment env result-type function &rest sequences)
  (declare (ignore result-type function))
  (flet ((get-type (x)
           (if (constantp x)
               (type-of x)
               (and (symbolp x)
                    (assoc 'type
                           (nth-value 2 (variable-information x env)))))))
    (if (every (lambda (seq)
                 (let ((type (get-type seq)))
                   (and type
                        (subtypep type 'sequence))))
               sequences)
        `(cl:map-into ,@(cdr form))
        form)))


;;
;; Function: reverse, nreverse
;; Generic Function: abstract-reverse

(defun reverse (sequence)
  (etypecase sequence
    (sequence (cl:reverse sequence))
    (abstract-sequence (abstract-reverse sequence))))
(define-typecase-compiler-macro reverse (sequence)
  (typecase sequence
    (sequence `(cl:reverse ,sequence))))

(defgeneric abstract-reverse (sequence)
  (:method ((sequence abstract-vector))
    (cl:reverse (coerce sequence 'vector)))
  (:method ((sequence abstract-list))
    (cl:reverse (coerce sequence 'list))))

(defun nreverse (sequence)
  (etypecase sequence
    (sequence (cl:nreverse sequence))
    (abstract-sequence (reverse sequence))))
(define-typecase-compiler-macro nreverse (sequence)
  (typecase sequence
    (sequence `(cl:nreverse ,sequence))))


;;
;; Function: reduce
;; Generic Function: abstract-reduce

(defun reduce (function sequence &rest args &key key from-end start end initial-value)
  (declare (ignore key from-end start end initial-value))
  (etypecase sequence
    (sequence (apply #'cl:reduce function sequence args))
    (abstract-sequence (apply #'abstract-reduce function sequence args))))
(define-typecase-compiler-macro reduce (&whole form function sequence &rest args)
  (typecase sequence
    (sequence `(cl:reduce ,@(cdr form)))))

(defgeneric abstract-reduce (function sequence &key key from-end start end initial-value)
  (:method (function (sequence abstract-vector) &rest args &key key from-end start end initial-value)
    (declare (ignore key from-end start end initial-value))
    (apply #'cl:reduce function (coerce sequence 'vector) args))
  (:method (function (sequence abstract-list) &rest args &key key from-end start end initial-value)
    (declare (ignore key from-end start end initial-value))
    (apply #'cl:reduce function (coerce sequence 'list) args)))


;;
;; Function: sort, stable-sort
;; Generic Function: abstract-sort, abstract-stable-sort

(defun sort (sequence pred &rest args &key key)
  (declare (ignore key))
  (etypecase sequence
    (sequence (apply #'cl:sort sequence pred args))
    (abstract-sequence (apply #'abstract-sort sequence pred args))))
(define-typecase-compiler-macro sort (&whole form sequence pred &rest args)
  (typecase sequence
    (sequence `(cl:sort ,@(cdr form)))))

(defgeneric abstract-sort (sequence pred &key key)
  (:method ((sequence abstract-vector) pred &rest args &key key)
    (declare (ignore key))
    (apply #'cl:sort (coerce sequence 'vector) pred args))
  (:method ((sequence abstract-list) pred &rest args &key key)
    (declare (ignore key))
    (apply #'cl:sort (coerce sequence 'list) pred args)))

(defun stable-sort (sequence pred &rest args &key key)
  (declare (ignore key))
  (etypecase sequence
    (sequence (apply #'cl:stable-sort sequence pred args))
    (abstract-sequence (apply #'abstract-stable-sort sequence pred args))))
(define-typecase-compiler-macro stable-sort (&whole form sequence pred &rest args)
  (typecase sequence
    (sequence `(cl:stable-sort ,@(cdr form)))))

(defgeneric abstract-stable-sort (sequence pred &key key)
  (:method ((sequence abstract-vector) pred &rest args &key key)
    (declare (ignore key))
    (apply #'cl:stable-sort (coerce sequence 'vector) pred args))
  (:method ((sequence abstract-list) pred &rest args &key key)
    (declare (ignore key))
    (apply #'cl:stable-sort (coerce sequence 'list) pred args)))


;;
;; Function: substitute, substitute-if, nsubstitute, nsubstitute-if
;; Generic Function: abstract-substitute, abstract-substitute-if

(defun substitute-if (new pred sequence &rest args &key from-end start end count key)
  (declare (ignore from-end start end count key))
  (etypecase sequence
    (sequence (apply #'cl:substitute-if new pred sequence args))
    (abstract-sequence (apply #'abstract-substitute-if new pred sequence args))))
(define-typecase-compiler-macro substitute-if (&whole form new pred sequence &rest args)
  (typecase sequence
    (sequence `(cl:substitute-if ,@(cdr form)))))

(defgeneric abstract-substitute-if (new pred sequence &key from-end start end count key)
  (:method (new pred (sequence abstract-vector) &rest args &key from-end start end count key)
    (declare (ignore from-end start end count key))
    (apply #'cl:substitute-if new pred (coerce sequence 'vector) args))
  (:method (new pred (sequence abstract-list) &rest args &key from-end start end count key)
    (declare (ignore from-end start end count key))
    (apply #'cl:substitute-if new pred (coerce sequence 'list) args)))

(defun substitute (new old sequence &rest args &key from-end start end key test count)
  (declare (ignore from-end start end key test count))
  (etypecase sequence
    (sequence (apply #'cl:substitute new old sequence args))
    (abstract-sequence (apply #'abstract-substitute new old sequence args))))
(define-typecase-compiler-macro substitute (&whole form new old sequence &rest args)
  (typecase sequence
    (sequence `(cl:substitute ,@(cdr form)))))

(defgeneric abstract-substitute (new old sequence &key from-end start end key test count)
  (:method (new old (sequence abstract-sequence) &rest args &key from-end start end key (test #'eql) count)
    (declare (ignore from-end start end key count))
    (setq args (delete-from-plist args :test))
    (apply #'substitute-if new (lambda (x) (funcall test old x)) sequence args)))

(defun nsubstitute-if (new pred sequence &rest args &key from-end start end count key)
  (declare (ignore from-end start end count key))
  (etypecase sequence
    (sequence (apply #'cl:nsubstitute-if new pred sequence args))
    (abstract-sequence (apply #'substitute-if new pred sequence args))))
(define-typecase-compiler-macro nsubstitute-if (&whole form new pred sequence &rest args)
  (typecase sequence
    (sequence `(cl:nsubstitute-if ,@(cdr form)))))

(defun nsubstitute (new old sequence &rest args &key from-end start end key test count)
  (declare (ignore from-end start end count key))
  (etypecase sequence
    (sequence (apply #'cl:nsubstitute new old sequence args))
    (abstract-sequence
     (setq args (delete-from-plist args :test))
     (apply #'nsubstitute-if new (lambda (x) (funcall test old x)) sequence args))))
(define-typecase-compiler-macro nsubstitute (&whole form new old sequence &rest args)
  (typecase sequence
    (sequence `(cl:nsubstitute ,@(cdr form)))))


;;
;; Function: split-sequence, split-sequence-if
;; Generic Function: abstract-split-sequence, abstract-split-sequence-if

(defun split-sequence (delimiter sequence &rest args &key start end from-end count remove-empty-subseqs test key)
  (declare (ignore start end from-end count remove-empty-subseqs test key))
  (etypecase sequence
    (sequence (apply #'split-sequence:split-sequence delimiter sequence args))
    (abstract-sequence (apply #'abstract-split-sequence delimiter sequence args))))
(define-typecase-compiler-macro split-sequence (&whole form delimiter sequence &rest args)
  (typecase sequence
    (sequence `(split-sequence:split-sequence ,@(cdr form)))))

(defgeneric abstract-split-sequence (delimiter sequence &key start end from-end count remove-empty-subseqs test key)
  (:method (delimiter (sequence abstract-list) &rest args &key start end from-end count remove-empty-subseqs (test #'eql test-specified-p) (key #'identity))
    (declare (ignore start end from-end count remove-empty-subseqs))
    (when test-specified-p
      (setq args (delete-from-plist args :test)))
    (apply #'abstract-split-sequence-if
           (lambda (x)
             (funcall test (funcall key x) delimiter))
           sequence
           args)))

(defun split-sequence-if (pred sequence &rest args &key start end from-end count remove-empty-subseqs key)
  (declare (ignore start end from-end count remove-empty-subseqs key))
  (etypecase sequence
    (sequence (apply #'split-sequence:split-sequence-if pred sequence args))
    (abstract-sequence (apply #'abstract-split-sequence-if pred sequence args))))
(define-typecase-compiler-macro split-sequence-if (&whole form pred sequence &rest args)
  (typecase sequence
    (sequence `(split-sequence:split-sequence-if ,@(cdr form)))))

(defgeneric abstract-split-sequence-if (pred sequence &key start end from-end count remove-empty-subseqs key)
  (:method (pred (sequence abstract-list) &rest args &key (start 0) end from-end count remove-empty-subseqs (key #'identity))
    (if (and (not from-end)
             (or end count))
        (loop with subseqs = '()
              with buf = '()
              with seq = (drop start sequence)
              with i = 0
              until (or (and count (zerop count))
                        (and end (<= end i))
                        (emptyp seq))
              for x = (first seq)
              when (funcall pred (funcall key x))
                do (when (or (null remove-empty-subseqs) buf)
                     (push (nreverse buf) subseqs)
                     (and count (decf count))
                     (setq buf nil))
              else
                do (push x buf)
              do (setf seq (rest seq))
                 (incf i)
              finally
                 (return (progn
                           (when buf (push (nreverse buf) subseqs))
                           (values
                            (cl:append (take start sequence)
                                       (nreverse subseqs))
                            (if (and remove-empty-subseqs
                                     count
                                     (zerop count))
                                (1+ i)
                                i)))))
        (apply #'split-sequence:split-sequence-if pred (coerce sequence 'list) args))))


;;
;; Function: remove-duplicates, delete-duplicates
;; Generic Function: abstract-remove-duplicates

(defun remove-duplicates (sequence &rest args &key from-end test start end key)
  (declare (ignore from-end test start end key))
  (etypecase sequence
    (sequence (apply #'cl:remove-duplicates sequence args))
    (abstract-sequence (apply #'abstract-remove-duplicates sequence args))))
(define-typecase-compiler-macro remove-duplicates (&whole form sequence &rest args)
  (typecase sequence
    (sequence `(cl:remove-duplicates ,@(cdr form)))))

(defgeneric abstract-remove-duplicates (sequence &key from-end test start end key)
  (:method ((sequence abstract-list) &rest args &key from-end test start end key)
    (declare (ignore from-end test start end key))
    (apply #'cl:remove-duplicates (coerce sequence 'list) args))
  (:method ((sequence abstract-vector) &rest args &key from-end test start end key)
    (declare (ignore from-end test start end key))
    (apply #'cl:remove-duplicates (coerce sequence 'vector) args)))

(defun delete-duplicates (sequence &rest args &key from-end test start end key)
  (declare (ignore from-end test start end key))
  (etypecase sequence
    (sequence (apply #'cl:delete-duplicates sequence args))
    (abstract-sequence (apply #'remove-duplicates sequence args))))
(define-typecase-compiler-macro delete-duplicates (&whole form sequence &rest args)
  (typecase sequence
    (sequence `(cl:delete-duplicates ,@(cdr form)))))


;;
;; Function: member, member-if
;; Generic Function: abstract-member, abstract-member-if

(defun member (item list &rest args &key key test)
  (declare (ignore key test))
  (etypecase list
    (list (apply #'cl:member item list args))
    (abstract-list (apply #'abstract-member item list args))))
(define-typecase-compiler-macro member (&whole form item list &rest args)
  (typecase list
    (list `(cl:member ,@(cdr form)))))

(defgeneric abstract-member (item list &key key test)
  (:method (item (list abstract-list) &key (key #'identity) (test #'eql))
    (drop-while (lambda (x)
                  (not (funcall test (funcall key x) item)))
                list)))

(defun member-if (test list &rest args &key key)
  (declare (ignore key))
  (etypecase list
    (list (apply #'cl:member-if test list args))
    (abstract-list (apply #'abstract-member-if test list args))))
(define-typecase-compiler-macro member-if (&whole form test list &rest args)
  (typecase list
    (list `(cl:member-if ,@(cdr form)))))

(defgeneric abstract-member-if (test list &key key)
  (:method (test (list abstract-list) &key key)
    (drop-while (if key
                    (lambda (x)
                      (not (funcall test (funcall key x))))
                    (complement test))
                list)))
