(in-package :cl-user)
(defpackage cl21.core.sequence
  (:use :cl)
  (:shadow :sequence
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
           :reverse
           :nreverse
           :reduce
           :sort
           :stable-sort
           :substitute
           :substitute-if
           :nsubstitute
           :nsubstitute-if
           :remove-duplicates
           :delete-duplicates
           :copy-seq
           :replace
           :fill
           :every
           :some
           :notevery
           :notany
           :map
           :append)
  (:shadowing-import-from :cl21.core.generic
                          :getf
                          :emptyp
                          :coerce)
  (:shadowing-import-from :cl21.core.array
                          :vector)
  (:import-from :cl21.core.condition
                :method-unimplemented-error)
  (:import-from :cl21.core.environment
                :variable-information)
  (:import-from :cl21.internal.util
                :define-typecase-compiler-macro)
  #+nil
  (:import-from :split-sequence
                :split-sequence
                :split-sequence-if)
  (:import-from :alexandria
                :delete-from-plist
                :with-gensyms)
  (:export :split
           :split-if
           :join

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
           :map-to

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
           :push-front
           :push-back
           :pushnew
           :pushnew-front
           :pushnew-back
           :pop
           :pop-front
           :pop-back
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

           :every
           :some
           :notevery
           :notany

           :take
           :drop
           :take-while
           :drop-while
           :partition
           :partition-if
           :subdivide
           :append

           :abstract-sequence
           :abstract-list
           :make-sequence-like
           :adjust-sequence
           :abstract-first
           :abstract-rest
           :abstract-elt
           :abstract-subseq
           :abstract-length
           :abstract-take
           :abstract-drop
           :abstract-take-while
           :abstract-drop-while
           :abstract-last
           :abstract-butlast
           :abstract-nbutlast
           :abstract-find-if
           :abstract-find
           :abstract-position-if
           :abstract-position
           :abstract-search
           :abstract-remove-if
           :abstract-remove
           :abstract-delete-if
           :abstract-delete
           :abstract-partition-if
           :abstract-subdivide
           :abstract-mismatch
           :abstract-count-if
           :abstract-count
           :abstract-reverse
           :abstract-nreverse
           :abstract-reduce
           :abstract-sort
           :abstract-stable-sort
           :abstract-substitute
           :abstract-substitute-if
           :abstract-nsubstitute
           :abstract-nsubstitute-if
           :abstract-remove-duplicates
           :abstract-delete-duplicates
           :abstract-split
           :abstract-split-if
           :abstract-copy-seq
           :abstract-replace
           :abstract-fill

           :make-sequence-iterator
           :iterator-pointer
           :iterator-endp
           :iterator-next
           :with-sequence-iterator))
(in-package :cl21.core.sequence)


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
                   (cl:sequence (apply (function ,cl-name) item sequence args))
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
                   (cl:sequence (list* ',cl-name (cdr form))))))
             nil))))


;;
;; Abstract Sequence

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass abstract-sequence () ())
  (defclass abstract-list (abstract-sequence) ())

  (deftype sequence () '(or cl:sequence abstract-sequence)))


;;; Basic methods

(defmethod emptyp ((object abstract-sequence))
  "Returns T if SEQUENCE is an empty sequence and NIL
otherwise."
  (zerop (abstract-length object)))

(defmethod getf ((place abstract-sequence) key &optional default)
  (if (< key (abstract-length place))
      (values (abstract-elt place key) t)
      (values default nil)))

(defmethod (setf getf) (newval (place abstract-sequence) key)
  (setf (abstract-elt place key) newval))

(defun length (sequence)
  #.(documentation 'cl:length 'function)
  (etypecase sequence
    (cl:sequence (cl:length sequence))
    (abstract-sequence (abstract-length sequence))))
(define-typecase-compiler-macro length (sequence)
  (typecase sequence
    (cl:sequence `(cl:length ,sequence))))

(defgeneric abstract-length (sequence)
  (:method ((sequence abstract-sequence))
    (method-unimplemented-error 'abstract-length sequence))

  (:documentation "Returns the length of SEQUENCE or signals a ABSTRACT-METHOD-UNIMPLEMENTED
error if the sequence method is not implemented for the class of
SEQUENCE."))

(defun elt (sequence index)
  #.(documentation 'cl:elt 'function)
  (etypecase sequence
    (cl:sequence (cl:elt sequence index))
    (abstract-sequence (abstract-elt sequence index))))
(define-typecase-compiler-macro elt (sequence index)
  (typecase sequence
    (cl:sequence `(cl:elt ,sequence ,index))))

(defgeneric abstract-elt (sequence index)
  (:method ((sequence abstract-sequence) index)
    (declare (ignore index))
    (method-unimplemented-error 'abstract-elt sequence))

  (:documentation "Returns the element at position INDEX of SEQUENCE or signals a
ABSTRACT-METHOD-UNIMPLMENETED error if the sequence method is not
implemented for the class of SEQUENCE."))

(defun (setf elt) (newval sequence index)
  #.(documentation '(setf cl:elt) 'function)
  (etypecase sequence
    (cl:sequence (setf (cl:elt sequence index) newval))
    (abstract-sequence (setf (abstract-elt sequence index) newval))))
(defgeneric (setf abstract-elt) (newval sequence index)
  (:method (newval (sequence abstract-sequence) index)
    (method-unimplemented-error '(setf abstract-elt) sequence))

  (:documentation "Replaces the element at position INDEX of SEQUENCE with NEWVAL
and returns NEWVAL or signals a ABSTRACT-METHOD-UNIMPLEMENTED error if
the sequence method is not implemented for the class of
SEQUENCE."))

(defun first (seq)
  #.(documentation 'cl:first 'function)
  (etypecase seq
    (cl:list (cl:first seq))
    (cl:sequence (cl:elt seq 0))
    (abstract-sequence (abstract-first seq))))
(define-typecase-compiler-macro first (seq)
  (typecase seq
    (cl:list `(cl:first ,seq))
    (cl:sequence `(cl:elt ,seq 0))))

(defun (setf first) (newval seq)
  #.(documentation '(setf cl:first) 'function)
  (etypecase seq
    (cl:list (setf (cl:first seq) newval))
    (cl:sequence (setf (cl:elt seq 0) newval))
    (abstract-sequence (setf (abstract-first seq) newval))))

(defgeneric abstract-first (seq)
  (:method ((seq abstract-sequence))
    (abstract-elt seq 0)))

(defgeneric (setf abstract-first) (newval seq)
  (:method (newval (seq abstract-sequence))
    (setf (abstract-elt seq 0) newval)))

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
             for cl-fun = (intern (string function) :cl)
             for abstract-fun = (intern (format nil "~A-~A" :abstract function))
             append `((defun ,function (seq)
                        ,(documentation cl-fun 'function)
                        (etypecase seq
                          (cl:list (,cl-fun seq))
                          (cl:sequence (cl:elt seq ,n))
                          (abstract-sequence (,abstract-fun seq))))
                      (defun (setf ,function) (newval seq)
                        ,(documentation `(setf ,cl-fun) 'function)
                        (etypecase seq
                          (cl:list (setf (,cl-fun seq) newval))
                          (cl:sequence (setf (cl:elt seq ,n) newval))
                          (abstract-sequence (setf (,abstract-fun seq) newval))))
                      (defgeneric ,abstract-fun (seq)
                        (:method ((seq abstract-sequence))
                          (abstract-elt seq ,n)))
                      (defgeneric (setf ,abstract-fun) (newval seq)
                        (:method (newval (seq abstract-sequence))
                          (setf (abstract-elt seq ,n) newval))))))

(defun rest (seq)
  #.(documentation 'cl:rest 'function)
  (etypecase seq
    (cl:list (cl:rest seq))
    (cl:sequence (cl:subseq seq 1))
    (abstract-sequence (abstract-rest seq))))
(define-typecase-compiler-macro rest (seq)
  (typecase seq
    (cl:list `(cl:rest ,seq))
    (cl:sequence `(drop 1 ,seq))))

(defgeneric abstract-rest (seq)
  (:method ((seq abstract-sequence))
    (abstract-subseq seq 1)))

(defun (setf rest) (newval seq)
  #.(documentation '(setf cl:rest) 'function)
  (etypecase seq
    (cl:list (setf (cl:rest seq) newval))
    (cl:sequence
     (replace seq newval :start1 1)
     (adjust-array seq (cl:length newval)))
    (abstract-sequence
     (setf (abstract-rest seq) newval))))

(defgeneric (setf abstract-rest) (newval seq)
  (:method (newval (seq abstract-sequence))
    (replace seq newval
             :start1 1)
    (adjust-sequence seq (1+ (length newval)))))

(defgeneric make-sequence-like (sequence length &key initial-element initial-contents)
  (:method ((seq cl:list) length &key (initial-element nil iep) (initial-contents nil icp))
    (cond
      ((and iep icp)
       (error "supplied both ~S and ~S to ~S" :initial-element :initial-contents 'make-sequence-like))
      (iep (make-list length :initial-element initial-element))
      (icp (unless (= (cl:length initial-contents) length)
             (error "length mismatch in ~S" 'make-sequence-like))
           (let ((result (make-list length)))
             (cl:replace result initial-contents)
             result))
      (t (make-list length))))
  (:method ((seq cl:vector) length &key (initial-element nil iep) (initial-contents nil icp))
    (cond
      ((and iep icp)
       (error "supplied both ~S and ~S to ~S" :initial-element :initial-contents 'make-sequence-like))
      (iep (make-array length :element-type (array-element-type seq)
                       :initial-element initial-element))
      (icp (make-array length :element-type (array-element-type seq)
                       :initial-contents initial-contents))
      (t (make-array length :element-type (array-element-type seq)))))
  (:method ((seq abstract-sequence) length &key initial-element initial-contents)
    (declare (ignore length initial-element initial-contents))
    (method-unimplemented-error 'make-sequence-like seq))

  (:documentation
   "Returns a freshly allocated sequence of length LENGTH and of the
same class as SEQUENCE. Elements of the new sequence are
initialized to INITIAL-ELEMENT, if supplied, initialized to
INITIAL-CONTENTS if supplied, or identical to the elements of
SEQUENCE if neither is supplied. Signals a ABSTRACT-METHOD-UNIMPLEMENTED
error if the sequence method is not implemented for the class of
SEQUENCE."))

(defgeneric adjust-sequence (sequence length &key initial-element initial-contents)
  (:method ((seq cl:list) length &key initial-element (initial-contents nil icp))
    (if (eql length 0)
        '()
        (let ((olength (length seq)))
          (cond
            ((eql length olength) (if icp
                                      (cl:replace seq initial-contents)
                                      seq))
            ((< length olength)
             (rplacd (nthcdr (1- length) seq) nil)
             (if icp (cl:replace seq initial-contents) seq))
            ((null seq)
             (let ((result (make-list length :initial-element initial-element)))
               (if icp
                   (cl:replace result initial-contents)
                   result)))
            (t (rplacd (nthcdr (1- olength) seq)
                       (make-list (- length olength)
                                  :initial-element initial-element))
               (if icp
                   (cl:replace seq initial-contents)
                   seq))))))
  (:method ((seq cl:vector) length &rest args &key (initial-contents nil icp) initial-element)
    (declare (ignore initial-element))
    (cond
      ((and (array-has-fill-pointer-p seq)
            (>= (array-total-size seq) length))
       (setf (fill-pointer seq) length)
       (if icp
           (cl:replace seq initial-contents)
           seq))
      ((eql (cl:length seq) length)
       (if icp
           (cl:replace seq initial-contents)
           seq))
      (t (let ((newseq (apply #'adjust-array seq length args)))
           (when (and (array-has-fill-pointer-p newseq)
                      (<= (fill-pointer newseq) length))
             (setf (fill-pointer newseq) length))
           newseq))))
  (:method ((seq abstract-sequence) length &key initial-element initial-contents)
    (declare (ignore length initial-element initial-contents))
    (method-unimplemented-error 'adjust-sequence seq))

  (:documentation "Return destructively modified SEQUENCE or a freshly allocated
sequence of the same class as SEQUENCE of length LENGTH. Elements
of the returned sequence are initialized to INITIAL-ELEMENT, if
supplied, initialized to INITIAL-CONTENTS if supplied, or identical
to the elements of SEQUENCE if neither is supplied. Signals a
ABSTRACT-METHOD-UNIMPLEMENTED error if the sequence method is not
implemented for the class of SEQUENCE."))


;; Iteration

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (sequence-iterator (:constructor %make-sequence-iterator (sequence &key pointer limit step)))
    (pointer 0 :type integer)
    (limit nil :type integer)
    (sequence nil :type (or cl:sequence abstract-sequence))
    (step +1 :type integer))

  (defgeneric make-sequence-iterator (sequence &key start end from-end)
    (:method ((sequence t) &key (start 0) end from-end)
      (let ((end (or end (length sequence))))
        (%make-sequence-iterator sequence
                                 :pointer (if from-end
                                              (1- end)
                                              start)
                                 :limit (if from-end
                                            (1- start)
                                            end)
                                 :step (if from-end
                                           -1
                                           +1)))))

  (defgeneric iterator-pointer (sequence-iterator)
    (:method ((iterator sequence-iterator))
      (sequence-iterator-pointer iterator)))

  (defgeneric iterator-next (sequence-iterator)
    (:method ((iterator sequence-iterator))
      (prog1
          (elt (sequence-iterator-sequence iterator)
               (sequence-iterator-pointer iterator))
        (incf (sequence-iterator-pointer iterator)
              (sequence-iterator-step iterator)))))

  (defgeneric iterator-endp (sequence-iterator)
    (:method ((iterator sequence-iterator))
      (= (sequence-iterator-pointer iterator)
         (sequence-iterator-limit iterator)))))

(defmacro with-sequence-iterator ((var iterator &rest result-form) (&optional (i (gensym "I"))) &body body)
  `(do ((,i (iterator-pointer ,iterator)
            (iterator-pointer ,iterator)))
       ((iterator-endp ,iterator) ,@result-form)
     ,(if var
          `(let ((,var (iterator-next ,iterator)))
             ,@body)
          `(progn (iterator-next ,iterator) ,@body))))

(defmacro do-abstract-sequence ((var sequence &rest result-form) (&optional (i (gensym "I")) (start 0) end from-end) &body body)
  (with-gensyms (iterator)
    `(let ((,iterator (make-sequence-iterator ,sequence :start ,start :end ,end :from-end ,from-end)))
       (with-sequence-iterator (,var ,iterator ,@result-form) (,i)
         ,@body))))


;;
;; coerce

(defmethod coerce ((object abstract-sequence) type)
  (ecase type
    (list
     (let ((results '()))
       (do-abstract-sequence (x object (cl:nreverse results)) ()
         (cl:push x results))))
    ((vector simple-vector)
     (let* ((length (abstract-length object))
            (result (make-array length)))
       (do-abstract-sequence (x object result) (i 0)
         (setf (aref result i) x))))
    (string
     (cl:coerce (coerce object 'vector) 'string))))


;;
;; Function: copy-seq
;; Generic Function: abstract-copy-seq

(defun copy-seq (sequence)
  #.(documentation 'cl:copy-seq 'function)
  (etypecase sequence
    (cl:sequence (cl:copy-seq sequence))
    (abstract-sequence (abstract-copy-seq sequence))))

(defgeneric abstract-copy-seq (sequence)
  (:method ((sequence abstract-sequence))
    (abstract-subseq sequence 0)))

;;
;; Macro: push, push-front, push-back
;; Generic Function: abstract-enque, abstract-enque-head, abstract-enque-tail

(defmacro _f2 (op arg1 place &rest args &environment env)
  (multiple-value-bind (vars vals binds set access)
      (get-setf-expansion place env)
    `(let* (,@(map #'list vars vals))
       (multiple-value-bind ,binds (,op ,arg1 ,access ,@args)
         ,set))))

(defun nenque (value sequence)
  (etypecase sequence
    (cl:list (nenque-head value sequence))
    (cl:vector (nenque-tail value sequence))
    (abstract-sequence (abstract-nenque value sequence))))

(defun nenque-head (value sequence)
  (etypecase sequence
    (cl:list (cl:cons value sequence))
    (cl:vector (let ((len (cl:length sequence)))
                 (unless (adjustable-array-p sequence)
                   (error "~S is not an adjustable array." sequence))
                 (if (= len (array-dimension sequence 0))
                     (adjust-array sequence (1+ len) :fill-pointer (1+ len))
                     (incf (fill-pointer sequence)))
                 (do ((i len (1- i)))
                     ((< i 1))
                   (setf (aref sequence i)
                         (aref sequence (1- i))))
                 (setf (aref sequence 0) value)
                 sequence))
    (abstract-sequence (abstract-nenque-head value sequence))))

(defun nenque-tail (value sequence)
  (etypecase sequence
    (cl:list (if sequence
                 (progn
                   (rplacd (cl:last sequence) (list value))
                   sequence)
                 (list value)))
    (cl:vector (cl:vector-push-extend value sequence)
               sequence)
    (abstract-sequence (abstract-nenque-tail value sequence))))

(defgeneric abstract-nenque (value sequence)
  (:method (value (sequence abstract-sequence))
    (abstract-nenque-tail value sequence))
  (:method (value (sequence abstract-list))
    (abstract-nenque-head value sequence)))

(defgeneric abstract-nenque-head (value sequence)
  (:method (value (sequence abstract-sequence))
    (setq sequence
          (adjust-sequence
           sequence (1+ (abstract-length sequence))))
    (abstract-replace sequence sequence :start1 1)
    (setf (abstract-elt sequence 0) value)
    sequence)
  (:method (value (sequence abstract-list))
    (let ((new (make-sequence-like sequence 1
                                   :initial-contents
                                   (list value))))
      (setf (abstract-rest new) sequence)
      new)))

(defgeneric abstract-nenque-tail (value sequence)
  (:method (value (sequence abstract-sequence))
    (let ((len (abstract-length sequence)))
      (adjust-sequence sequence (1+ len))
      (setf (abstract-elt sequence len) value)
      sequence))
  (:method (value (sequence abstract-list))
    (if (emptyp sequence)
        (make-sequence-like sequence 1
                            :initial-contents (list value))
        (let ((last
               (do ((current sequence (abstract-rest current)))
                   ((emptyp (abstract-rest current)) current))))
          (setf (abstract-rest last)
                (make-sequence-like sequence 1
                                    :initial-contents
                                    (list value)))
          sequence))))

(defmacro push (value place)
  #.(documentation 'cl:push 'function)
  `(_f2 nenque ,value ,place))

(defmacro push-front (value place)
  #.(documentation 'cl:push 'function)
  `(_f2 nenque-head ,value ,place))

(defmacro push-back (value place)
  `(_f2 nenque-tail ,value ,place))


;;
;; Macro: pushnew, pushnew-front, pushnew-back
;; Generic Function: abstract-nenque-new

(defun nenque-new (value sequence &rest args)
  (etypecase sequence
    (cl:list (apply #'nenque-head-new value sequence args))
    (cl:vector (apply #'nenque-tail-new value sequence args))
    (abstract-sequence (apply #'abstract-nenque-new value sequence args))))

(defun nenque-head-new (value sequence &rest args)
  (if (apply #'find value sequence args)
      sequence
      (nenque-head value sequence)))

(defun nenque-tail-new (value sequence &rest args)
  (if (apply #'find value sequence args)
      sequence
      (nenque-tail value sequence)))

(defgeneric abstract-nenque-new (value sequence &rest args)
  (:method (value (sequence abstract-sequence) &rest args)
    (apply #'nenque-tail-new value sequence args))
  (:method (value (sequence abstract-list) &rest args)
    (apply #'nenque-head-new value sequence args)))

(defmacro pushnew (value place &rest keys &key key test)
  #.(or (documentation 'cl:pushnew 'function) "")
  (declare (ignore key test))
  `(_f2 nenque-new ,value ,place ,@keys))

(defmacro pushnew-front (value place &rest keys &key key test)
  (declare (ignore key test))
  `(_f2 nenque-head-new ,value ,place ,@keys))

(defmacro pushnew-back (value place &rest keys &key key test)
  (declare (ignore key test))
  `(_f2 nenque-tail-new ,value ,place ,@keys))


;;
;; Macro: pop, pop-front, pop-back
;; Generic Function: abstract-ndequeue, abstract-ndequeue-head, abstract-ndequeue-tail

(defun ndequeue (sequence)
  (etypecase sequence
    (cl:list (ndequeue-head sequence))
    (cl:vector (ndequeue-tail sequence))
    (abstract-sequence (abstract-ndequeue sequence))))

(defun ndequeue-head (sequence)
  (etypecase sequence
    (cl:list (values (cl:cdr sequence)
                     (cl:car sequence)))
    (cl:vector
     (unless (adjustable-array-p sequence)
       (error "~S is not an adjustable array." sequence))
     (let ((len (cl:length sequence))
           (ret (aref sequence 0)))
       (do ((i 0 (1+ i)))
           ((= i (1- len)))
         (setf (aref sequence i)
               (aref sequence (1+ i))))
       (decf (fill-pointer sequence))
       (values sequence
               ret)))
    (abstract-sequence (abstract-ndequeue-head sequence))))

(defun ndequeue-tail (sequence)
  (etypecase sequence
    (cl:list
     (let* ((last-cons (cl:last sequence 2))
            (last (cl:second last-cons)))
       (rplacd last-cons nil)
       (values sequence last)))
    (cl:vector
     (values sequence
             (cl:vector-pop sequence)))
    (abstract-sequence
     (abstract-ndequeue-tail sequence))))

(defgeneric abstract-ndequeue (sequence)
  (:method ((sequence abstract-sequence))
    (abstract-ndequeue-tail sequence))
  (:method ((sequence abstract-list))
    (abstract-ndequeue-head sequence)))

(defgeneric abstract-ndequeue-head (sequence)
  (:method ((sequence abstract-sequence))
    (let ((ret (abstract-first sequence)))
      (abstract-replace sequence sequence :start2 1)
      (values
       (adjust-sequence sequence
                        (1- (abstract-length sequence)))
       ret))))

(defgeneric abstract-ndequeue-tail (sequence)
  (:method ((sequence abstract-sequence))
    (let* ((len (abstract-length sequence))
           (ret (abstract-elt sequence (1- len))))
      (adjust-sequence sequence (1- len))
      (values sequence ret))))

(defmacro pop (place &environment env)
  #.(documentation 'cl:pop 'function)
  (multiple-value-bind (vars vals binds set access)
      (get-setf-expansion place env)
    (with-gensyms (place-g ret)
      `(let* (,@(map #'list vars vals)
              (,place-g ,access))
         (multiple-value-bind (,@binds ,ret)
             (ndequeue ,place-g)
           ,set
           ,ret)))))

(defmacro pop-front (place &environment env)
  (multiple-value-bind (vars vals binds set access)
      (get-setf-expansion place env)
    (with-gensyms (place-g ret)
      `(let* (,@(map #'list vars vals)
              (,place-g ,access))
         (multiple-value-bind (,@binds ,ret)
             (ndequeue-head ,place-g)
           ,set
           ,ret)))))

(defmacro pop-back (place &environment env)
  (multiple-value-bind (vars vals binds set access)
      (get-setf-expansion place env)
    (with-gensyms (place-g ret)
      `(let* (,@(map #'list vars vals)
              (,place-g ,access))
         (multiple-value-bind (,@binds ,ret)
             (ndequeue-tail ,place-g)
           ,set
           ,ret)))))


;;
;; Function: subseq, (setf subseq)
;; Generic Function: abstract-subseq, (setf abstract-subseq)

(defun cl21-subseq (sequence start &optional end)
  (let ((len (cl:length sequence)))
    (flet ((real-index (idx)
             (if (and idx (< idx 0))
                 (setf idx (- len (mod (abs idx) len)))
                 idx)))
      (cl:subseq sequence (real-index start) (real-index end)))))

(defun subseq (sequence start &optional end)
  #.(documentation 'cl:subseq 'function)
  (etypecase sequence
    (cl:sequence (cl21-subseq sequence start end))
    (abstract-sequence (abstract-subseq sequence start end))))
(define-typecase-compiler-macro subseq (&whole form sequence start &optional end)
  (typecase sequence
    (cl:sequence `(cl21-subseq ,@(cdr form)))))

(defgeneric abstract-subseq (sequence start &optional end)
  (:method ((sequence abstract-sequence) start &optional end)
    (let* ((end (or end
                    (abstract-length sequence)))
           (result (make-sequence-like sequence (- end start))))
      (if (zerop (- end start))
          result
          (do-abstract-sequence (x sequence result) (i start end)
            (setf (abstract-elt result (- i start)) x))))))

(defun (setf subseq) (newval sequence start &optional end)
  #.(documentation '(setf cl:subseq) 'function)
  (etypecase sequence
    (cl:sequence (setf (cl:subseq sequence start end) newval))
    (abstract-sequence (setf (abstract-subseq sequence start end) newval))))
(define-typecase-compiler-macro (setf subseq) (&whole form sequence start &optional end)
  (typecase sequence
    (cl:sequence `(setf (cl:subseq ,@(cddr form)) ,(cadr form)))))

(defgeneric (setf abstract-subseq) (newval sequence start &optional end)
  (:method (newval (sequence abstract-sequence) start &optional end)
    (replace sequence newval :start1 start :end1 end)))


;;
;; Function: replace
;; Generic Function: abstract-replace

(defun replace (sequence1 sequence2 &rest args &key start1 end1 start2 end2)
  #.(or (documentation 'cl:replace 'function) "")
  (declare (ignore start1 end1 start2 end2))
  (if (and (typep sequence1 'cl:sequence)
           (typep sequence2 'cl:sequence))
      (apply #'cl:replace sequence1 sequence2 args)
      (apply #'abstract-replace sequence1 sequence2 args)))
;; TODO: compiler-macro

(defgeneric abstract-replace (sequence1 sequence2 &key start1 end1 start2 end2)
  ;; XXX: inefficient if the sequence is an abstract-list.
  (:method (sequence1 sequence2 &key (start1 0) end1 (start2 0) end2)
    (let ((end1 (or end1
                    (length sequence1)))
          (end2 (or end2
                    (length sequence2)))
          (setpairs '()))
      (do ((i start1 (1+ i))
           (j start2 (1+ j)))
          ((or (<= end1 i)
               (<= end2 j)))
        (cl:push (elt sequence2 j) setpairs)
        (cl:push `(elt ,sequence1 ,i) setpairs))
      (eval `(setf ,@setpairs))
      sequence1)))


;;
;; Function: fill
;; Generic Function: abstract-fill

(defun fill (sequence item &rest args &key start end)
  (declare (ignore start end))
  (etypecase sequence
    (cl:sequence (apply #'cl:fill sequence item args))
    (abstract-sequence (apply #'abstract-fill sequence item args))))
(define-typecase-compiler-macro fill (&whole form sequence item &key start end)
  (typecase sequence
    (cl:sequence `(cl:fill ,@(cdr form)))))

(defgeneric abstract-fill (sequence item &key start end)
  (:method ((sequence abstract-sequence) item &key (start 0) end)
    (do-abstract-sequence (() sequence sequence) (i start end)
      (setf (abstract-elt sequence i) item))))


;;
;; Function: take, drop
;; Generic Function: abstract-take, abstract-drop

(defun take (n sequence)
  (etypecase sequence
    (cl:sequence (cl:subseq sequence 0 n))
    (abstract-sequence (abstract-take n sequence))))
(define-typecase-compiler-macro take (n sequence)
  (typecase sequence
    (cl:sequence `(cl:subseq ,sequence 0 ,n))))

(defgeneric abstract-take (n sequence)
  (:method (n (sequence abstract-sequence))
    (abstract-subseq sequence 0 n)))

(defun drop (n sequence)
  (etypecase sequence
    (cl:list (nthcdr n sequence))
    (cl:sequence (cl:subseq sequence n))
    (abstract-sequence (abstract-drop n sequence))))
(define-typecase-compiler-macro drop (n sequence)
  (typecase sequence
    (cl:list `(nthcdr ,n ,sequence))
    (cl:sequence `(cl:subseq ,sequence ,n))))

(defgeneric abstract-drop (n sequence)
  (:method (n (sequence abstract-sequence))
    (abstract-subseq sequence n)))


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
    (cl:sequence (%sequence-take-while pred sequence))
    (abstract-sequence (abstract-take-while pred sequence))))
(define-typecase-compiler-macro take-while (pred sequence)
  (typecase sequence
    (sequence `(%sequence-take-while ,pred ,sequence))))

(defgeneric abstract-take-while (pred sequence)
  (:method (pred (sequence abstract-sequence))
    (do-abstract-sequence (x sequence) (i 0)
      (unless (funcall pred x)
        (return (abstract-subseq sequence 0 (1- i)))))))

(defun %sequence-drop-while (pred sequence)
  (let ((pos (cl:position-if (complement pred) sequence)))
    (if pos
        (cl:subseq sequence pos)
        (cl:subseq sequence 0 0))))

(defun drop-while (pred sequence)
  (etypecase sequence
    (cl:sequence (%sequence-drop-while pred sequence))
    (abstract-sequence (abstract-drop-while pred sequence))))
(define-typecase-compiler-macro drop-while (pred sequence)
  (typecase sequence
    (cl:sequence `(%sequence-drop-while ,pred ,sequence))))

(defgeneric abstract-drop-while (pred sequence)
  (:method (pred (sequence abstract-sequence))
    (do-abstract-sequence (x sequence) (i 0)
      (unless (funcall pred x)
        (return (abstract-subseq sequence i))))))


;;
;; Function: last
;; Generic Function: abstract-last

(defun last (sequence)
  #.(documentation 'cl:last 'function)
  (etypecase sequence
    (cl:list (car (cl:last sequence)))
    (cl:sequence (cl:elt sequence (1- (cl:length sequence))))
    (abstract-sequence (abstract-last sequence))))
(define-typecase-compiler-macro last (sequence)
  (typecase sequence
    (cl:list `(car (cl:last ,sequence)))
    (cl:sequence `(cl:elt ,sequence (1- (cl:length ,sequence))))))

(defgeneric abstract-last (sequence)
  (:method ((sequence abstract-sequence))
    (abstract-elt sequence (1- (abstract-length sequence)))))


;;
;; Function: butlast, nbutlast
;; Generic Function: abstract-butlast, abstract-nbutlast

(defun butlast (sequence &optional (n 1))
  #.(documentation 'cl:butlast 'function)
  (assert (>= n 0))
  (etypecase sequence
    (cl:list (cl:butlast sequence n))
    (cl:sequence (cl:subseq sequence 0 (- (cl:length sequence) n)))
    (abstract-sequence (abstract-butlast sequence n))))
(define-typecase-compiler-macro butlast (sequence &optional n)
  (typecase sequence
    (cl:list `(cl:butlast ,sequence ,(or n 1)))
    (cl:sequence `(cl:subseq ,sequence 0 (- (cl:length ,sequence) ,(or n 1))))))

(defgeneric abstract-butlast (sequence &optional n)
  (:method ((sequence abstract-sequence) &optional (n 1))
    (abstract-nbutlast (abstract-copy-seq sequence) n))
  (:method ((sequence abstract-sequence) &optional (n 1))
    (abstract-subseq sequence 0 (- (abstract-length sequence) n))))

(defun nbutlast (sequence &optional (n 1))
  #.(documentation 'cl:nbutlast 'function)
  (etypecase sequence
    (cl:list (cl:nbutlast sequence n))
    (abstract-sequence (abstract-nbutlast sequence n))))
(define-typecase-compiler-macro nbutlast (sequence &optional n)
  (typecase sequence
    (cl:list `(cl:nbutlast ,sequence ,(or n 1)))))

(defgeneric abstract-nbutlast (sequence &optional n)
  (:method ((sequence abstract-sequence) &optional (n 1))
    (adjust-sequence sequence (- (abstract-length sequence) n))))


;;
;; Function: find, find-if
;; Generic Function: abstract-find, abstract-find-if

(defun find-if (pred sequence &rest args &key from-end start end key)
  #.(or (documentation 'cl:find-if 'function) "")
  (declare (ignore from-end start end key))
  (etypecase sequence
    (cl:sequence (apply #'cl:find-if pred sequence args))
    (abstract-sequence (apply #'abstract-find-if pred sequence args))))
(define-typecase-compiler-macro find-if (&whole form pred sequence &rest args)
  (typecase sequence
    (cl:sequence `(cl:find-if ,@(cdr form)))))

(defgeneric abstract-find-if (pred sequence &key from-end start end key)
  (:method (pred (sequence abstract-sequence) &key from-end (start 0) end (key #'identity))
    (do-abstract-sequence (x sequence) (i start end from-end)
      (when (funcall pred (funcall key x))
        (return x)))))

(define-sequence-function-by-if find find-if)


;;
;; Function: position, position-if
;; Generic Function: abstract-position, abstract-position-if

(defun position-if (pred sequence &rest args &key from-end start end key)
  #.(or (documentation 'cl:position-if 'function) "")
  (declare (ignore from-end start end key))
  (etypecase sequence
    (cl:sequence (apply #'cl:position-if pred sequence args))
    (abstract-sequence (apply #'abstract-position-if pred sequence args))))
(define-typecase-compiler-macro position-if (&whole form pred sequence &rest args)
  (typecase sequence
    (cl:sequence `(apply #'cl:position-if ,pred ,sequence ,@(cdddr form)))))

(defgeneric abstract-position-if (pred sequence &key from-end start end key)
  (:method (pred (sequence abstract-sequence) &key from-end (start 0) end (key #'identity))
    (do-abstract-sequence (x sequence) (i start end from-end)
      (when (funcall pred (funcall key x))
        (return i)))))

(define-sequence-function-by-if position position-if)


;;
;; Function: search
;; Generic Function: abstract-search

(defun search (sequence1 sequence2 &rest args &key from-end test start1 end1 start2 end2 key)
  #.(or (documentation 'cl:search 'function) "")
  (declare (ignore from-end test start1 end1 start2 end2 key))
  (etypecase sequence2
    (cl:sequence (apply #'cl:search sequence1 sequence2 args))
    (abstract-sequence (apply #'abstract-search sequence1 sequence2 args))))
(define-typecase-compiler-macro search (&whole form sequence1 sequence2 &rest args)
  (typecase sequence2
    (cl:sequence `(cl:search ,@(cdr form)))))

(defgeneric abstract-search (sequence1 sequence2 &key from-end test start1 end1 start2 end2 key)
  (:method (sequence1 (sequence2 abstract-sequence) &key from-end (test #'eql) (start1 0) end1 (start2 0) end2 (key #'identity))
    (when (typep sequence1 'abstract-sequence)
      (setq sequence1 (coerce sequence1 'vector)))
    (setq sequence1 (cl:subseq sequence1 start1 end1))
    (let ((length (cl:length sequence1))
          (first-el (funcall key (cl:elt sequence1 0))))
      (do-abstract-sequence (x sequence2) (i start2 end2 from-end)
        (when (and (funcall test (funcall key x) first-el)
                   (or (not from-end) (>= i length))
                   (cl:every (lambda (x y)
                               (funcall test (funcall key x) (funcall key y)))
                             sequence1
                             (coerce
                              (if from-end
                                  (abstract-subseq sequence2 (- i length) i)
                                  (abstract-subseq sequence2 i (+ i length)))
                              'vector)))
          (return i))))))


;;
;; Function: remove, remove-if, remove-if-not, delete, delete-if, delete-if-not, keep, keep-if, nkeep, nkeep-if
;; Generic Function: abstract-remove, abstract-remove-if, abstract-delete, abstract-delete-if

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun remove-if (pred sequence &rest args &key from-end start end count key)
    #.(or (documentation 'cl:remove-if 'function) "")
    (declare (ignore from-end start end count key))
    (etypecase sequence
      (cl:sequence (apply #'cl:remove-if pred sequence args))
      (abstract-sequence (apply #'abstract-remove-if pred sequence args))))

  (defgeneric abstract-remove-if (pred sequence &key from-end start end count key)
    (:method (pred (sequence abstract-sequence) &rest args &key from-end start end count key)
      (declare (ignore from-end start end count key))
      (apply #'abstract-delete-if pred (abstract-copy-seq sequence) args)))
  (define-typecase-compiler-macro remove-if (&whole form pred sequence &rest args)
    (typecase sequence
      (cl:sequence `(cl:remove-if ,@(cdr form)))))

  (define-sequence-function-by-if remove remove-if)

  (defun remove-if-not (pred sequence &rest args &key from-end start end count key)
    #.(or (documentation 'cl:remove-if-not 'function) "")
    (declare (ignore from-end start end count key))
    (etypecase sequence
      (cl:sequence (apply #'cl:remove-if-not pred sequence args))
      (abstract-sequence
       (apply #'abstract-remove-if (complement pred) sequence args))))
  (define-typecase-compiler-macro remove-if-not (&whole form pred sequence &rest args)
    (typecase sequence
      (cl:sequence `(cl:remove-if-not ,@(cdr form)))))

  (defun delete-if (pred sequence &rest args &key from-end start end count key)
    #.(or (documentation 'cl:delete-if 'function) "")
    (declare (ignore from-end start end count key))
    (etypecase sequence
      (cl:sequence (apply #'cl:delete-if pred sequence args))
      (abstract-sequence
       (apply #'abstract-delete-if pred sequence args))))
  (define-typecase-compiler-macro delete-if (&whole form pred sequence &rest args)
    (typecase sequence
      (cl:sequence `(cl:delete-if ,@(cdr form)))))

  (define-sequence-function-by-if delete delete-if)

  (defgeneric abstract-delete-if (pred sequence &key from-end start end count key)
    (:method (pred (sequence abstract-sequence) &key from-end (start 0) end count (key #'identity))
      (let* ((removed-count 0)
             (loop-end (or end
                           (abstract-length sequence)))
             (index (if from-end
                        loop-end
                        0)))
        (do-abstract-sequence (x sequence) (i start loop-end from-end)
          (if (and (not (eql removed-count count))
                   (funcall pred (funcall key x)))
              (incf removed-count)
              (progn
                (setf (abstract-elt sequence index) x)
                (if from-end
                    (decf index)
                    (incf index)))))
        (if from-end
            (replace sequence sequence
                     :start1 start :end1 (- (length sequence) removed-count)
                     :start2 (+ start removed-count) :end2 (length sequence))
            (unless (or (null end)
                        (= end (length sequence)))
              (replace sequence sequence
                       :start1 (- end removed-count) :end1 (- (length sequence) removed-count)
                       :start2 end)))
        (adjust-sequence sequence (- (length sequence) removed-count)))))

  (defun delete-if-not (pred sequence &rest args &key from-end (start 0) end count (key #'identity))
    #.(or (documentation 'cl:delete-if-not 'function) "")
    (declare (ignore from-end start end count key))
    (etypecase sequence
      (cl:sequence (apply #'cl:delete-if-not args))
      (abstract-sequence
       (apply #'abstract-delete-if (complement pred) sequence args))))
  (define-typecase-compiler-macro delete-if-not (&whole form pred sequence &rest args)
    (typecase sequence
      (cl:sequence `(cl:delete-if-not ,@(cdr form)))))

  (setf (symbol-function 'keep-if) #'remove-if-not)
  (define-typecase-compiler-macro keep-if (&whole form pred sequence &rest args)
    (typecase sequence
      (cl:sequence `(cl:remove-if-not ,@(cdr form)))))

  (setf (symbol-function 'nkeep-if) #'delete-if-not)
  (define-typecase-compiler-macro nkeep-if (&whole form pred sequence &rest args)
    (typecase sequence
      (cl:sequence `(cl:delete-if-not ,@(cdr form)))))

  (define-sequence-function-by-if keep keep-if nil)

  (define-sequence-function-by-if nkeep nkeep-if nil))

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
                (nreverse sequence)
                sequence))
    (values yes no)))

(defun partition-if (pred sequence &rest args &key from-end start end key)
  "Given a predicate PRED, partition SEQUENCE into two sublists, the first
of which has elements that satisfy PRED, the second which do not."
  (declare (ignore from-end start end key))
  (etypecase sequence
    (cl:sequence (apply #'%partition-if pred sequence args))
    (abstract-sequence (apply #'abstract-partition-if pred sequence args))))

(defgeneric abstract-partition-if (pred sequence &key from-end start end key)
  (:method (pred (sequence abstract-sequence) &rest args &key from-end start end key)
    (declare (ignore from-end start end key))
    (apply #'%partition-if pred sequence args)))

(define-sequence-function-by-if partition partition-if)


;;
;; Function: subdivide
;; Generic Function: abstract-subdivide

;; TODO: from-end
;; TODO: Should return multiple values for indicating how long characters are traversed, just like split-sequence?
(defun subdivide (sequence chunk-size &rest args &key (start 0) end count)
  "Split `sequence` into subsequences of size `chunk-size`."
  (check-type chunk-size (integer 1))

  (etypecase sequence
    (cl:list
     (setq sequence (cl:subseq sequence start end))
     (if count
         (loop repeat count
               while sequence
               collect
               (loop repeat chunk-size
                     while sequence
                     collect (cl:pop sequence)))
         (loop while sequence
               collect
               (loop repeat chunk-size
                     while sequence
                     collect (cl:pop sequence)))))

    (cl:sequence
     (setq sequence (cl:subseq sequence start end))
     (if count
         (loop with len = (cl:length sequence)
               with end = (min len (* count chunk-size))
               for i below end by chunk-size
               collect (cl:subseq sequence i (min end (+ chunk-size i))))
         (loop with len = (cl:length sequence)
               for i below len by chunk-size
               collect (cl:subseq sequence i (min len (+ chunk-size i))))))
    (abstract-sequence (apply #'abstract-subdivide sequence chunk-size args) )))

;; TODO: Rewrite without using `coerce'.
(defgeneric abstract-subdivide (sequence chunk-size &key start end count)
  (:method ((sequence abstract-sequence) chunk-size &rest args &key start end count)
    (declare (ignore start end count))
    (mapcar (lambda (div)
              (make-sequence-like sequence (cl:length div)
                                  :initial-contents div))
            (apply #'subdivide (coerce sequence 'list) chunk-size args))))


;;
;; Function: mismatch
;; Generic Function: abstract-mismatch

(defun mismatch (sequence1 sequence2 &rest args &key from-end test start1 end1 start2 end2 key)
  #.(or (documentation 'cl:mismatch 'function) "")
  (declare (ignore from-end test start1 end1 start2 end2 key))
  (if (and (typep sequence1 'cl:sequence)
           (typep sequence2 'cl:sequence))
      (apply #'cl:mismatch sequence1 sequence2 args)
      (apply #'abstract-mismatch sequence1 sequence2 args)))
;; TODO: compiler-macro

(defgeneric abstract-mismatch (sequence1 sequence2 &key from-end test start1 end1 start2 end2 key)
  (:method ((sequence1 abstract-sequence) (sequence2 cl:sequence) &rest args &key from-end test start1 end1 start2 end2 key)
    (declare (ignore from-end test start1 end1 start2 end2 key))
    (apply #'abstract-mismatch sequence2 sequence1 args))
  (:method (sequence1 (sequence2 abstract-sequence) &key from-end (test #'eql) (start1 0) end1 (start2 0) end2 (key #'identity))
    (let* ((end1 (or end1
                     (length sequence1)))
           (end2 (or end2
                     (length sequence2)))
           (current (if from-end
                        (1- end2)
                        start2)))
      (do-abstract-sequence (x sequence1 (cond
                                           ((if from-end
                                                (= current (1- start2))
                                                (= current end2)) nil)
                                           ((= (- end2 start2) (- i start1)) nil)
                                           (from-end (- end1 i))
                                           (t i)))
          (i start1 end1 from-end)
        (when (if from-end
                  (= current (1- start2))
                  (= current end2))
          (return (if from-end
                      (1+ i)
                      (1- i))))
        (unless (funcall test (funcall key x) (funcall key (elt sequence2 current)))
          (return (if from-end
                      (- end1 i)
                      i)))
        (incf current (if from-end -1 1))))))


;;
;; Function: count, count-if
;; Generic Function: abstract-count, abstract-count-if

(defun count-if (pred sequence &rest args &key from-end start end key)
  #.(or (documentation 'cl:count-if 'function) "")
  (declare (ignore from-end start end key))
  (etypecase sequence
    (cl:sequence (apply #'cl:count-if pred sequence args))
    (abstract-sequence (apply #'abstract-count-if pred sequence args))))
(define-typecase-compiler-macro count-if (&whole form pred sequence &rest args)
  (typecase sequence
    (cl:sequence `(cl:count-if ,@(cdr form)))))

(defgeneric abstract-count-if (pred sequence &key from-end start end key)
  (:method (pred (sequence abstract-sequence) &key from-end (start 0) end (key #'identity))
    (let ((count 0))
      (do-abstract-sequence (x sequence count) (i start end from-end)
        (when (funcall pred (funcall key x))
          (incf count))))))

(define-sequence-function-by-if count count-if)


;;
;; Function: reverse, nreverse
;; Generic Function: abstract-reverse, abstract-nreverse

(defun reverse (sequence)
  #.(documentation 'cl:reverse 'function)
  (etypecase sequence
    (cl:sequence (cl:reverse sequence))
    (abstract-sequence (abstract-reverse sequence))))
(define-typecase-compiler-macro reverse (sequence)
  (typecase sequence
    (cl:sequence `(cl:reverse ,sequence))))

(defgeneric abstract-reverse (sequence)
  (:method ((sequence abstract-sequence))
    (abstract-nreverse (abstract-copy-seq sequence))))

(defun nreverse (sequence)
  #.(documentation 'cl:nreverse 'function)
  (etypecase sequence
    (cl:sequence (cl:nreverse sequence))
    (abstract-sequence (abstract-nreverse sequence))))
(define-typecase-compiler-macro nreverse (sequence)
  (typecase sequence
    (cl:sequence `(cl:nreverse ,sequence))))

(defgeneric abstract-nreverse (sequence)
  (:method ((sequence abstract-sequence))
    (do ((i 0 (1+ i))
         (j (1- (abstract-length sequence)) (1- j)))
        ((>= i j) sequence)
      (psetf (abstract-elt sequence i) (abstract-elt sequence j)
             (abstract-elt sequence j) (abstract-elt sequence i)))))


;;
;; Function: reduce
;; Generic Function: abstract-reduce

(defun reduce (function sequence &rest args &key key from-end start end initial-value)
  #.(or (documentation 'cl:reduce 'function) "")
  (declare (ignore key from-end start end initial-value))
  (etypecase sequence
    (cl:sequence (apply #'cl:reduce function sequence args))
    (abstract-sequence (apply #'abstract-reduce function sequence args))))
(define-typecase-compiler-macro reduce (&whole form function sequence &rest args)
  (typecase sequence
    (cl:sequence `(cl:reduce ,@(cdr form)))))

(defgeneric abstract-reduce (function sequence &key key from-end start end initial-value)
  (:method (function (sequence abstract-sequence) &key (key #'identity) from-end (start 0) end (initial-value nil ivp))
    (if (and (not ivp) (emptyp sequence))
        (funcall function)
        (let ((end (and from-end
                        (or end
                            (abstract-length sequence))))
              (current initial-value))
          (unless ivp
            (setq current (abstract-elt sequence (if from-end (1- end) start)))
            (if from-end
                (decf end)
                (incf start)))
          (do-abstract-sequence (x sequence current)
              (i start end from-end)
            (setq current
                  (if from-end
                      (funcall function
                               (funcall key x)
                               current)
                      (funcall function
                               current
                               (funcall key x)))))))))


;;
;; Function: sort, stable-sort
;; Generic Function: abstract-sort, abstract-stable-sort

(defun sort (sequence pred &rest args &key key)
  #.(or (documentation 'cl:sort 'function) "")
  (declare (ignore key))
  (etypecase sequence
    (cl:sequence (apply #'cl:sort sequence pred args))
    (abstract-sequence (apply #'abstract-sort sequence pred args))))
(define-typecase-compiler-macro sort (&whole form sequence pred &rest args)
  (typecase sequence
    (cl:sequence `(cl:sort ,@(cdr form)))))

(defgeneric abstract-sort (sequence pred &key key)
  (:method ((sequence abstract-sequence) pred &rest args &key key)
    (declare (ignore key))
    (let ((length (abstract-length sequence)))
      (make-sequence-like sequence length
                          :initial-contents
                          (apply #'cl:sort (coerce sequence 'list) pred args)))))

(defun stable-sort (sequence pred &rest args &key key)
  #.(or (documentation 'cl:stable-sort 'function) "")
  (declare (ignore key))
  (etypecase sequence
    (cl:sequence (apply #'cl:stable-sort sequence pred args))
    (abstract-sequence (apply #'abstract-stable-sort sequence pred args))))
(define-typecase-compiler-macro stable-sort (&whole form sequence pred &rest args)
  (typecase sequence
    (cl:sequence `(cl:stable-sort ,@(cdr form)))))

(defgeneric abstract-stable-sort (sequence pred &key key)
  (:method ((sequence abstract-sequence) pred &rest args &key key)
    (declare (ignore key))
    (let ((length (abstract-length sequence)))
      (make-sequence-like sequence length
                          :initial-contents
                          (apply #'cl:stable-sort (coerce sequence 'list) pred args)))))


;;
;; Function: substitute, substitute-if, nsubstitute, nsubstitute-if
;; Generic Function: abstract-substitute, abstract-substitute-if

(defun substitute-if (new pred sequence &rest args &key from-end start end count key)
  #.(or (documentation 'cl:substitute-if 'function) "")
  (declare (ignore from-end start end count key))
  (etypecase sequence
    (cl:sequence (apply #'cl:substitute-if new pred sequence args))
    (abstract-sequence (apply #'abstract-substitute-if new pred sequence args))))
(define-typecase-compiler-macro substitute-if (&whole form new pred sequence &rest args)
  (typecase sequence
    (cl:sequence `(cl:substitute-if ,@(cdr form)))))

(defgeneric abstract-substitute-if (new pred sequence &key from-end start end count key)
  (:method (new pred (sequence abstract-sequence) &rest args &key from-end start end count key)
    (declare (ignore from-end start end count key))
    (apply #'abstract-nsubstitute-if new pred (abstract-copy-seq sequence) args)))

(defun substitute (new old sequence &rest args &key from-end start end key test count)
  #.(or (documentation 'cl:substitute 'function) "")
  (declare (ignore from-end start end key test count))
  (etypecase sequence
    (cl:sequence (apply #'cl:substitute new old sequence args))
    (abstract-sequence (apply #'abstract-substitute new old sequence args))))
(define-typecase-compiler-macro substitute (&whole form new old sequence &rest args)
  (typecase sequence
    (cl:sequence `(cl:substitute ,@(cdr form)))))

(defgeneric abstract-substitute (new old sequence &key from-end start end key test count)
  (:method (new old (sequence abstract-sequence) &rest args &key from-end start end key (test #'eql) count)
    (declare (ignore from-end start end key count))
    (setq args (delete-from-plist args :test))
    (apply #'substitute-if new (lambda (x) (funcall test old x)) sequence args)))

(defun nsubstitute-if (new pred sequence &rest args &key from-end start end count key)
  #.(or (documentation 'cl:nsubstitute-if 'function) "")
  (declare (ignore from-end start end count key))
  (etypecase sequence
    (cl:sequence (apply #'cl:nsubstitute-if new pred sequence args))
    (abstract-sequence (apply #'abstract-nsubstitute-if new pred sequence args))))
(define-typecase-compiler-macro nsubstitute-if (&whole form new pred sequence &rest args)
  (typecase sequence
    (cl:sequence `(cl:nsubstitute-if ,@(cdr form)))))

(defgeneric abstract-nsubstitute-if (new pred sequence &key from-end start end count key)
  (:method (new pred (sequence abstract-sequence) &key from-end (start 0) end count (key #'identity))
    (let ((subst-count 0))
      (do-abstract-sequence (x sequence sequence) (i start end from-end)
        (when (eql count subst-count)
          (return sequence))
        (when (funcall pred (funcall key x))
          (incf subst-count)
          (setf (abstract-elt sequence i) new))))))

(defun nsubstitute (new old sequence &rest args &key from-end start end key test count)
  #.(or (documentation 'cl:nsubstitute 'function) "")
  (declare (ignore from-end start end count key))
  (etypecase sequence
    (cl:sequence (apply #'cl:nsubstitute new old sequence args))
    (abstract-sequence
     (setq args (delete-from-plist args :test))
     (apply #'nsubstitute-if new (lambda (x) (funcall test old x)) sequence args))))
(define-typecase-compiler-macro nsubstitute (&whole form new old sequence &rest args)
  (typecase sequence
    (cl:sequence `(cl:nsubstitute ,@(cdr form)))))

(defgeneric abstract-nsubstitute (new old sequence &key from-end start end key test count)
  (:method (new old (sequence abstract-sequence) &rest args &key from-end start end key (test #'eql) count)
    (declare (ignore from-end start end key count))
    (setq args (delete-from-plist args :test))
    (apply #'nsubstitute-if new (lambda (x) (funcall test old x)) sequence args)))


;;
;; Function: split, split-if
;; Generic Function: abstract-split, abstract-split-if

(defun split (delimiter sequence &rest args &key start end from-end count remove-empty-subseqs test key)
  #.(or (documentation 'split-sequence:split-sequence 'function) "")
  (declare (ignore start end from-end count remove-empty-subseqs test key))
  (etypecase sequence
    (cl:sequence (apply #'split-sequence:split-sequence delimiter sequence args))
    (abstract-sequence (apply #'abstract-split delimiter sequence args))))
(define-typecase-compiler-macro split (&whole form delimiter sequence &rest args)
  (typecase sequence
    (cl:sequence `(split-sequence:split-sequence ,@(cdr form)))))

(defgeneric abstract-split (delimiter sequence &key start end from-end count remove-empty-subseqs test key)
  (:method (delimiter (sequence abstract-sequence) &rest args &key start end from-end count remove-empty-subseqs (test #'eql test-specified-p) (key #'identity))
    (declare (ignore start end from-end count remove-empty-subseqs))
    (when test-specified-p
      (setq args (delete-from-plist args :test)))
    (apply #'abstract-split-if
           (lambda (x)
             (funcall test (funcall key x) delimiter))
           sequence
           args)))

(defun split-if (pred sequence &rest args &key start end from-end count remove-empty-subseqs key)
  #.(or (documentation 'split-sequence:split-sequence-if 'function) "")
  (declare (ignore start end from-end count remove-empty-subseqs key))
  (etypecase sequence
    (cl:sequence (apply #'split-sequence:split-sequence-if pred sequence args))
    (abstract-sequence (apply #'abstract-split-if pred sequence args))))
(define-typecase-compiler-macro split-if (&whole form pred sequence &rest args)
  (typecase sequence
    (cl:sequence `(split-sequence:split-sequence-if ,@(cdr form)))))

(defgeneric abstract-split-if (pred sequence &key start end from-end count remove-empty-subseqs key)
  (:method (pred (sequence abstract-sequence) &key (start 0) end from-end count remove-empty-subseqs (key #'identity))
    (let ((subseqs '())
          (buf '())
          (split-counts 0))
      (flet ((final ()
               (when buf
                 (cl:push (make-sequence-like sequence (cl:length buf)
                                              :initial-contents (if from-end
                                                                    buf
                                                                    (cl:nreverse buf)))
                          subseqs))
               (cl:nreverse subseqs)))
        (do-abstract-sequence (x sequence (values (final) i)) (i start end from-end)
          (when (eql count split-counts)
            (return (values (final) i)))
          (if (funcall pred (funcall key x))
              (when (or (not remove-empty-subseqs)
                        buf)
                (cl:push
                 (make-sequence-like sequence (cl:length buf)
                                     :initial-contents (if from-end
                                                           buf
                                                           (cl:nreverse buf)))
                 subseqs)
                (incf split-counts)
                (setq buf nil))
              (cl:push x buf)))))))


;;
;; Function: join

(defun join (joint list-of-sequences)
  (etypecase (car list-of-sequences)
    (cl:list (loop for (x . xs) on list-of-sequences
                   append x
                   when xs
                     collect joint))
    (cl:string (with-output-to-string (s)
                 (loop for (x . xs) on list-of-sequences
                       do (princ x s)
                       when xs
                         do (write-char joint s))))
    ((or cl:vector
         abstract-sequence)
     (let* ((length (loop for (x . xs) on list-of-sequences
                          summing (length x)
                          when xs
                            summing 1))
            (result (make-sequence-like (car list-of-sequences)
                                        length)))
       (loop with i = 0
             for (seq . xs) on list-of-sequences
             do (do-abstract-sequence (x seq) ()
                  (setf (elt result i) x)
                  (incf i))
             when xs
               do (setf (elt result i) joint)
                  (incf i))
       result))))


;;
;; Function: remove-duplicates, delete-duplicates
;; Generic Function: abstract-remove-duplicates

(defun remove-duplicates (sequence &rest args &key from-end test start end key)
  #.(or (documentation 'cl:remove-duplicates 'function) "")
  (declare (ignore from-end test start end key))
  (etypecase sequence
    (cl:sequence (apply #'cl:remove-duplicates sequence args))
    (abstract-sequence (apply #'abstract-remove-duplicates sequence args))))
(define-typecase-compiler-macro remove-duplicates (&whole form sequence &rest args)
  (typecase sequence
    (cl:sequence `(cl:remove-duplicates ,@(cdr form)))))

(defgeneric abstract-remove-duplicates (sequence &key from-end test start end key)
  (:method ((sequence abstract-sequence) &rest args &key from-end test start end key)
    (declare (ignore from-end test start end key))
    (apply #'abstract-delete-duplicates (abstract-copy-seq sequence) args)))

(defun delete-duplicates (sequence &rest args &key from-end test start end key)
  #.(or (documentation 'cl:delete-duplicates 'function) "")
  (declare (ignore from-end test start end key))
  (etypecase sequence
    (cl:sequence (apply #'cl:delete-duplicates sequence args))
    (abstract-sequence (apply #'abstract-delete-duplicates sequence args))))
(define-typecase-compiler-macro delete-duplicates (&whole form sequence &rest args)
  (typecase sequence
    (cl:sequence `(cl:delete-duplicates ,@(cdr form)))))

(defgeneric abstract-delete-duplicates (sequence &key from-end test start end key)
  (:method ((sequence abstract-sequence) &key from-end (test #'eql) (start 0) end (key #'identity))
    (let* ((length (abstract-length sequence))
           (end (or end length))
           (step (if from-end -1 +1))
           (pointer (if from-end
                        (1- end)
                        start))
           (deleted-count 0))
      (do-abstract-sequence (x sequence
                               (when from-end
                                 ;; shift items "deleted-count" to the left
                                 (do ((i deleted-count (1+ i))
                                      (j start (1+ j)))
                                     ((= i end))
                                   (setf (abstract-elt sequence j)
                                         (abstract-elt sequence i))))
                               (unless (= end length)
                                 (do ((i end (1+ i)))
                                     ((= i length))
                                   (setf (abstract-elt sequence (- i deleted-count))
                                         (abstract-elt sequence i))))
                               (adjust-sequence sequence (- length deleted-count)))
          (i start end from-end)
        (let ((x (funcall key x)))
          (do ((j (+ i step) (+ j step)))
              ((if from-end
                   (= j (1- start))
                   (= j end))
               (setf (abstract-elt sequence pointer) x)
               (incf pointer step))
            (when (funcall test
                           x
                           (funcall key (abstract-elt sequence j)))
              (incf deleted-count)
              (return))))))))


;;
;; Function: every, some, notevery, notany

(defmacro do-sequences ((x sequences &rest result-form) &body body)
  (let ((iterators (gensym "ITERATORS")))
    `(let ((,iterators (mapcar #'make-sequence-iterator ,sequences)))
       (do ()
           ((cl:some #'iterator-endp ,iterators) ,@result-form)
         ,(if x
              `(let ((,x (mapcar #'iterator-next ,iterators)))
                 ,@body)
              `(progn (mapc #'iterator-next ,iterators)
                      ,@body))))))

(defun every (pred &rest sequences)
  (do-sequences (x sequences t)
    (unless (apply pred x)
      (return nil))))

(defun some (pred &rest sequences)
  (do-sequences (x sequences nil)
    (when (apply pred x)
      (return t))))

(defun notevery (pred &rest sequences)
  (do-sequences (x sequences nil)
    (unless (apply pred x)
      (return t))))

(defun notany (pred &rest sequences)
  (do-sequences (x sequences t)
    (when (apply pred x)
      (return nil))))


;;
;; Function: map, map-to

(defun map-to (type function &rest sequences)
  (cond
    ((eq type 'string)
     (with-output-to-string (s)
       (do-sequences (x sequences)
         (write-char (apply function x) s))))
    ((eq type 'cl:hash-table)
     (let ((hash (make-hash-table :test 'equal)))
       (do-sequences (x sequences)
         (let ((res (apply function x)))
           (setf (gethash (car res) hash) (cdr res))))
       hash))
    ((or (eq type 'list)
         (subtypep type 'abstract-list))
     (let ((results '())
           (length 0))
       (do-sequences (x sequences)
         (cl:push (apply function x) results)
         (incf length))
       (if (eq type 'list)
           (nreverse results)
           (make-sequence-like (allocate-instance type) length
                               :initial-contents (nreverse results)))))
    ((or (subtypep type 'vector)
         (subtypep type 'abstract-sequence))
     (let ((min-len 0))
       (do-sequences (nil sequences)
         (incf min-len))
       (let ((result (if (subtypep type 'vector)
                         (make-array min-len)
                         (make-sequence-like (allocate-instance type) min-len)))
             (pointer 0))
         (do-sequences (x sequences)
           (setf (elt result pointer) (apply function x))
           (incf pointer))
         result)))
    (T (error "Invalid type specifier: ~S" type))))

(defun map (function &rest sequences)
  (let ((type (etypecase (car sequences)
                (string 'string)
                (hash-table 'hash-table)
                (vector 'vector)
                (list 'list)
                (abstract-sequence (type-of (car sequences))))))
    (apply #'map-to type function sequences)))


;;
;; Function: append

(defun append (&rest sequences)
  (flet ((append-to-list (sequences)
           (let* ((results (cons nil nil))
                  (current results))
             (dolist (sequence sequences)
               (if (typep sequence 'sequence)
                   (do-abstract-sequence (x sequence) ()
                     (setf (cdr current) (cons x nil))
                     (setq current (cdr current)))
                   (progn
                     (setf (cdr current) sequence)
                     (setq current (cdr current)))))
             (cdr results)))
         (append-to-vector (sequences)
           (let* ((len (reduce #'+ sequences :key #'length))
                  (result (make-array len))
                  (i 0))
             (dolist (sequence sequences result)
               (do-abstract-sequence (x sequence) ()
                 (setf (aref result i) x)
                 (incf i)))))
         (append-to-string (sequences)
           (with-output-to-string (s)
             (dolist (sequence sequences)
               (do-abstract-sequence (x sequence) ()
                 (write-char x s))))))
    (etypecase (car sequences)
      (string (append-to-string sequences))
      (list (append-to-list sequences))
      (vector (append-to-vector sequences))
      (abstract-sequence
       (let ((result (append-to-vector sequences)))
         (make-sequence-like (car sequences)
                             (cl:length result)
                             :initial-contents (cl:coerce result 'list)))))))
