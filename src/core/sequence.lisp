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
           :replace)
  (:shadowing-import-from :cl21.core.generic
                          :emptyp
                          :coerce)
  (:import-from :cl21.core.environment
                :variable-information)
  (:import-from :cl21.core.util
                :define-typecase-compiler-macro)
  #+nil
  (:import-from :split-sequence
                :split-sequence
                :split-sequence-if)
  (:import-from :alexandria
                :delete-from-plist
                :ensure-list
                :with-gensyms)
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
           :abstract-split-sequence
           :abstract-split-sequence-if
           :abstract-copy-seq
           :abstract-replace))
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
                   (cl:sequence (list* 'apply (function ,cl-name) item sequence (cdddr form))))))
             nil))))


;;
;; Abstract Sequence

(defclass abstract-sequence () ())
(defclass abstract-list (abstract-sequence) ())
(defclass abstract-vector (abstract-sequence) ())


;;; Error

(define-condition abstract-method-unimplemented (simple-error)
  ((class-name :type symbol
               :initarg :class-name)
   (method-name :type (or symbol list)
                :initarg :method-name))
  (:documentation "An error raised if the abstract-sequence class has not specified a mandatory method.")
  (:report
   (lambda (condition stream)
     (format stream
             "Method ~S for ~S must be implemented."
             (slot-value condition 'method-name)
             (slot-value condition 'class-name)))))

(defun method-unimplemented-error (method-name sequence)
  (error 'abstract-method-unimplemented
         :class-name (class-name (class-of sequence))
         :method-name method-name))


;;; Basic methods

(defmethod emptyp ((object abstract-sequence))
  "Returns T if SEQUENCE is an empty sequence and NIL
otherwise."
  (zerop (abstract-length object)))

(defun length (sequence)
  #.(documentation 'cl:length 'function)
  (etypecase sequence
    (cl:sequence (cl:length sequence))
    (abstract-sequence (abstract-length sequence))))
(define-typecase-compiler-macro length (sequence)
  (typecase sequence
    (cl:sequence `(cl:length ,sequence))))

(defgeneric abstract-length (sequence)
  (:method ((sequence abstract-list))
    (do-abstract-sequence (() sequence i) (i)))
  (:method ((sequence abstract-vector))
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
  (:method ((sequence abstract-list) index)
    (abstract-first (drop index sequence)))
  (:method ((sequence abstract-vector) index)
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
  (:method (newval (sequence abstract-list) index)
    (setf (abstract-first (%nthrest index sequence)) newval))

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
  (:method ((seq abstract-vector))
    (abstract-elt seq 0))
  (:method ((seq abstract-list))
    (method-unimplemented-error 'abstract-first seq)))

(defgeneric (setf abstract-first) (newval seq)
  (:method (newval (seq abstract-vector))
    (setf (abstract-elt seq 0) newval))
  (:method (newval (seq abstract-list))
    (declare (ignore newval))
    (method-unimplemented-error '(setf abstract-first) seq)))

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
  (:method ((seq abstract-vector))
    (abstract-subseq seq 1))
  (:method ((seq abstract-list))
    (method-unimplemented-error 'abstract-rest seq)))

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
  (:method (newval (seq abstract-vector))
    (replace seq newval
             :start1 1)
    (adjust-sequence seq (1+ (length newval))))
  (:method (newval (seq abstract-list))
    (declare (ignore newval))
    (method-unimplemented-error '(setf abstract-rest) seq)))

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
      (t (apply #'adjust-array seq length args))))
  (:method ((seq abstract-list) length &key initial-element (initial-contents nil icp))
    (if (eql length 0)
        (make-sequence-like seq 0)
        (let ((olength (abstract-length seq)))
          (cond
            ((eql length olength)
             (if icp
                 (replace seq initial-contents)
                 seq))
            ((< length olength)
             (setf (%nthrest length seq) (make-sequence-like seq 0))
             (if icp
                 (replace seq initial-contents)
                 seq))
            ((zerop olength)
             (let ((result (make-sequence-like seq length :initial-element initial-element)))
               (if icp
                   (replace result initial-contents)
                   result)))
            (t (setf (%nthrest olength seq)
                     (make-sequence-like seq
                                         (- length olength)
                                         :initial-element initial-element))
               (if icp
                   (replace seq initial-contents)
                   seq))))))
  (:method ((seq abstract-vector) length &key initial-element initial-contents)
    (declare (ignore length initial-element initial-contents))
    (method-unimplemented-error 'adjust-sequence seq))

  (:documentation "Return destructively modified SEQUENCE or a freshly allocated
sequence of the same class as SEQUENCE of length LENGTH. Elements
of the returned sequence are initialized to INITIAL-ELEMENT, if
supplied, initialized to INITIAL-CONTENTS if supplied, or identical
to the elements of SEQUENCE if neither is supplied. Signals a
ABSTRACT-METHOD-UNIMPLEMENTED error if the sequence method is not
implemented for the class of SEQUENCE."))

(defmethod coerce ((object abstract-vector) type)
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

(defmethod coerce ((object abstract-list) type)
  (ecase type
    (list
     (let ((results '()))
       (do-abstract-sequence (x object (cl:nreverse results)) ()
         (cl:push x results))))
    ((vector simple-vector)
     (apply #'vector (coerce object 'list)))
    (string
     (cl:coerce (coerce object 'list) 'string))))


;; Iteration

(defgeneric iterator-next (sequence iterator)
  (:method ((sequence cl:vector) iterator)
    (values
     (cl:aref sequence iterator)
     sequence
     sequence))
  (:method ((sequence cl:list) iterator)
    (declare (ignore iterator))
    (values
     (car sequence)
     (cdr sequence)
     sequence))
  (:method ((sequence abstract-vector) iterator)
    (values
     (abstract-elt sequence iterator)
     sequence
     sequence))
  (:method ((sequence abstract-list) iterator)
    (declare (ignore iterator))
    (values
     (abstract-first sequence)
     (abstract-rest sequence)
     sequence)))

(defgeneric iterator-endp (sequence iterator start end from-end)
  (:method ((sequence cl:vector) iterator start end from-end)
    (eql iterator (if from-end (1- start) end)))
  (:method ((sequence cl:list) iterator start end from-end)
    (or (eql iterator (if from-end (1- start) end))
        (null sequence)))
  (:method ((sequence abstract-vector) iterator start end from-end)
    (eql iterator (if from-end (1- start) end)))
  (:method ((sequence abstract-list) iterator start end from-end)
    (or (eql iterator (if from-end (1- start) end))
        (emptyp sequence))))

(defmacro do-abstract-vector ((vars sequence &rest result-form) (&optional (i (gensym "I")) (start 0) end from-end) &body body)
  (with-gensyms (endsymb)
    `(let ((,endsymb (or ,end (length ,sequence))))
       (do ((,i (if ,from-end (1- ,endsymb) ,start) (+ ,i (if ,from-end -1 +1))))
           ((iterator-endp ,sequence ,i ,start ,endsymb ,from-end) ,@result-form)
         (multiple-value-bind ,(ensure-list vars) (iterator-next ,sequence ,i)
           ,@body)))))

(defmacro do-abstract-list ((vars sequence &rest result-form) (&optional (i (gensym "I")) (start 0) end from-end) &body body)
  (with-gensyms (seq buf first rest all)
    (let ((from-end-form `(let ((,buf '()))
                            (do-abstract-list (,first ,sequence) (,i ,start ,end)
                              (cl:push ,first ,buf))
                            (do-abstract-list (,vars ,buf ,@result-form) (,i)
                              ,@body)))
          (normal-form `(let ((,seq ,sequence))
                          (dotimes (,i ,start)
                            (setq ,seq (nth-value 1 (iterator-next ,seq ,i))))
                          (do ((,i ,start (1+ ,i)))
                              ((iterator-endp ,seq ,i ,start ,end nil) ,@result-form)
                            (multiple-value-bind (,first ,rest ,all) (iterator-next ,seq ,i)
                              (multiple-value-bind ,(ensure-list vars) (values ,first ,rest ,all)
                                ,@body
                                (setq ,seq ,rest)))))))
      (if (constantp from-end)
          (if from-end
              from-end-form
              normal-form)
          `(if ,from-end
               ,from-end-form
               ,normal-form)))))

(defmacro do-abstract-sequence ((vars sequence &rest result-form) (&optional (i (gensym "I")) (start 0) end from-end) &body body)
  `(etypecase ,sequence
     ((or cl:list abstract-list)
      (do-abstract-list (,vars ,sequence ,@result-form) (,i ,start ,end ,from-end)
        ,@body))
     ((or cl:vector abstract-vector)
      (do-abstract-vector (,vars ,sequence ,@result-form) (,i ,start ,end ,from-end)
        ,@body))))

;; Generic version of nthcdr.
;; These 2 functions should be in core.cons, but the package depends on this package.
(defun %nthrest (i sequence)
  (if (zerop i)
      sequence
      (dotimes (n (1- i) (rest sequence))
        (setq sequence (rest sequence)))))

(defun (setf %nthrest) (newrest i sequence)
  (when (> i 1)
    (dotimes (n (1- i))
      (setq sequence (rest sequence))))
  (setf (rest sequence) newrest))


;;
;; Function: copy-seq
;; Generic Function: abstract-copy-seq

(defun copy-seq (sequence)
  #.(documentation 'cl:copy-seq 'function)
  (etypecase sequence
    (cl:sequence (cl:copy-seq sequence))
    (abstract-sequence (abstract-copy-seq sequence))))

(defgeneric abstract-copy-seq (sequence)
  (:method ((sequence abstract-vector))
    (abstract-subseq sequence 0))
  (:method ((sequence abstract-list))
    (cond
      ((emptyp sequence) (make-sequence-like sequence 0))
      ((emptyp (abstract-rest sequence))
       (make-sequence-like sequence 1
                           :initial-contents (list (abstract-first sequence))))
      (T (let ((new (make-sequence-like sequence 1)))
           (setf (abstract-first new) (abstract-first sequence)
                 (abstract-rest new) (copy-seq (abstract-rest sequence)))
           new)))))


;;
;; Macro: push, pushnew, pop

(defmacro push (value place)
  #.(documentation 'cl:push 'function)
  (let ((place-g (gensym "PLACE")))
    `(let ((,place-g ,place))
       (etypecase ,place-g
         (list (cl:push ,value ,place))
         (vector (vector-push-extend ,value ,place-g))))))

(defmacro pushnew (value place &rest keys &key key test test-not)
  #.(or (documentation 'cl:push 'function) "")
  (declare (ignore key test test-not))
  (let ((place-g (gensym "PLACE")))
    `(let ((,place-g ,place))
       (etypecase ,place-g
         (cl:vector (or (cl:find ,value ,place-g ,@keys)
                        (vector-push-extend ,value ,place-g)))
         (cl:list (cl:pushnew ,value ,place-g ,@keys))))))

(defmacro pop (place)
  #.(documentation 'cl:pop 'function)
  (let ((place-g (gensym "PLACE")))
    `(let ((,place-g ,place))
       (etypecase ,place-g
         (cl:vector (cl:vector-pop ,place-g))
         (cl:list (cl:pop ,place))
         (abstract-list
          (prog1
           (abstract-first ,place-g)
           (setf ,place (abstract-rest ,place-g))))))))


;;
;; Function: subseq, (setf subseq)
;; Generic Function: abstract-subseq, (setf abstract-subseq)

(defun subseq (sequence start &optional end)
  #.(documentation 'cl:subseq 'function)
  (etypecase sequence
    (cl:sequence (cl:subseq sequence start end))
    (abstract-sequence (abstract-subseq sequence start end))))
(define-typecase-compiler-macro subseq (&whole form sequence start &optional end)
  (typecase sequence
    (cl:sequence `(cl:subseq ,@(cdr form)))))

(defgeneric abstract-subseq (sequence start &optional end)
  (:method ((sequence abstract-vector) start &optional end)
    (let* ((end (or end
                    (abstract-length sequence)))
           (result (make-sequence-like sequence (- end start))))
      (if (zerop (- end start))
          result
          (do-abstract-sequence (x sequence result) (i start end)
            (setf (abstract-elt result (- i start)) x)))))
  (:method ((sequence abstract-list) start &optional end)
    (let ((results '()))
      (do-abstract-sequence (x sequence (make-sequence-like sequence (- i start)
                                                            :initial-contents
                                                            (cl:nreverse results)))
          (i start end)
        (cl:push x results)))))

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
                    (length sequence2))))
      (do ((i start1 (1+ i))
           (j start2 (1+ j)))
          ((or (<= end1 i)
               (<= end2 j)))
        (setf (elt sequence1 i) (elt sequence2 j))))))


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
  (:method (n (sequence abstract-vector))
    (abstract-subseq sequence 0 n))
  (:method (n (sequence abstract-list))
    (let ((results '()))
      (do-abstract-sequence (x sequence (make-sequence-like sequence n
                                                            :initial-contents
                                                            (cl:nreverse results)))
          (i 0 n)
        (cl:push x results)))))

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
  (:method (n (sequence abstract-vector))
    (abstract-subseq sequence n))
  (:method (n (sequence abstract-list))
    (copy-seq (%nthrest n sequence))))


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
  (:method (pred (sequence abstract-vector))
    (do-abstract-sequence (x sequence) (i 0)
      (unless (funcall pred x)
        (return (abstract-subseq sequence 0 (1- i))))))
  (:method (pred (sequence abstract-list))
    (let ((results '()))
      (flet ((final (length)
               (make-sequence-like sequence length
                                   :initial-contents (cl:nreverse results))))
        (do-abstract-sequence (x sequence (final i)) (i)
          (unless (funcall pred x)
            (return (final i)))
          (cl:push x results))))))

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
  (:method (pred (sequence abstract-vector))
    (do-abstract-sequence (x sequence) (i 0)
      (unless (funcall pred x)
        (return (abstract-subseq sequence i)))))
  (:method (pred (sequence abstract-list))
    (do-abstract-sequence ((x xs xxs) sequence) (i 0)
      (declare (ignore xs))
      (unless (funcall pred x)
        (return xxs)))))


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
  (:method ((sequence abstract-vector))
    (abstract-elt sequence (1- (abstract-length sequence))))
  (:method ((sequence abstract-list))
    (do-abstract-sequence ((x xs) sequence) ()
      (when (emptyp xs)
        (return x)))))


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
  (:method ((sequence abstract-vector) &optional (n 1))
    (abstract-subseq sequence 0 (- (abstract-length sequence) n)))
  (:method ((sequence abstract-list) &optional (n 1))
    (abstract-nbutlast (abstract-copy-seq sequence) n)))

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
  (:method (pred (sequence abstract-vector) &key from-end (start 0) end (key #'identity))
    (do-abstract-sequence (x sequence) (i start end from-end)
      (when (funcall pred (funcall key x))
        (return x))))
  (:method (pred (sequence abstract-list) &key from-end (start 0) end (key #'identity))
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
  (:method (pred (sequence abstract-vector) &key from-end (start 0) end (key #'identity))
    (do-abstract-sequence (x sequence) (i start end from-end)
      (when (funcall pred (funcall key x))
        (return i))))
  (:method (pred (sequence abstract-list) &key from-end (start 0) end (key #'identity))
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
  (:method (sequence1 (sequence2 abstract-vector) &key from-end (test #'eql) (start1 0) end1 (start2 0) end2 (key #'identity))
    (when (typep sequence1 'abstract-sequence)
      (setq sequence1 (coerce sequence1 'vector)))
    (setq sequence1 (cl:subseq sequence1 start1 end1))
    (let ((length (cl:length sequence1))
          (first-el (funcall key (cl:elt sequence1 0))))
      (do-abstract-sequence (x sequence2) (i start2 end2 from-end)
        (when (and (funcall test (funcall key x) first-el)
                   (or (not from-end) (>= i length))
                   (every (lambda (x y)
                            (funcall test (funcall key x) (funcall key y)))
                          sequence1
                          (coerce
                           (if from-end
                               (abstract-subseq sequence2 (- i length) i)
                               (abstract-subseq sequence2 i (+ i length)))
                           'vector)))
          (return i)))))
  (:method (sequence1 (sequence2 abstract-list) &key from-end (test #'eql) (start1 0) end1 (start2 0) end2 (key #'identity))
    (when (typep sequence1 'abstract-sequence)
      (setq sequence1 (coerce sequence1 'list)))
    (setq sequence1 (subseq sequence1 start1 end1))
    (let ((length (cl:length sequence1))
          (first-el (funcall key (car sequence1))))
      (do-abstract-sequence ((x xs) sequence2) (i start2 end2 from-end)
        (when (and (funcall test (funcall key x) first-el)
                   (every (lambda (x y)
                            (funcall test (funcall key x) (funcall key y)))
                          (cdr sequence1)
                          (coerce (take (1- length) xs) 'list)))
          (return i))))))


;;
;; Function: remove, remove-if, remove-if-not, delete, delete-if, delete-if-not, keep, keep-if, nkeep, nkeep-if
;; Generic Function: abstract-remove, abstract-remove-if, abstract-delete, abstract-delete-if

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
  (:method (pred (sequence abstract-vector) &key from-end (start 0) end count (key #'identity))
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
      (adjust-sequence sequence (- (length sequence) removed-count))))
  (:method (pred (sequence abstract-list) &key from-end (start 0) end count (key #'identity))
    (let ((removed-count 0)
          (buf '()))
      (if from-end
          (do-abstract-sequence ((x xs) sequence
                                 (make-sequence-like sequence (- end i removed-count)
                                                     :initial-contents (cl:nreverse buf)))
              (i start end t)
            (when (eql removed-count count)
              (cl:push x buf)
              (let* ((length (length xs))
                     (result
                       (make-sequence-like sequence (+ i length)
                                           :initial-contents (cl:nreverse (nconc buf xs)))))
                (return-from abstract-delete-if result)))
            (if (funcall pred (funcall key x))
                (incf removed-count)
                (cl:push x buf)))
          (do-abstract-sequence ((x xs xxs) sequence
                                 (make-sequence-like sequence (- i start removed-count)
                                                     :initial-contents (cl:nreverse buf)))
              (i start end)
            (declare (ignore xs))
            (when (eql removed-count count)
              (let ((result (make-sequence-like sequence (- i start removed-count)
                                                :initial-contents (cl:nreverse buf))))
                (loop with last = result
                      until (emptyp (abstract-rest last))
                      do (setq last (abstract-rest last))
                      finally
                         (setf (abstract-rest last) xxs))
                (return result)))
            (if (funcall pred (funcall key x))
                (incf removed-count)
                (cl:push x buf)))))))

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

(define-sequence-function-by-if nkeep nkeep-if nil)

(defun %partition-if (pred sequence &key from-end (start 0) end (key #'identity))
  (let ((yes nil)
        (no nil)
        (sequence (subseq sequence start end)))
    (map nil
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
  (:method (sequence1 (sequence2 abstract-vector) &key from-end (test #'eql) (start1 0) end1 (start2 0) end2 (key #'identity))
    (let* ((end1 (or end1
                     (length sequence1)))
           (end2 (or end2
                     (length sequence2)))
           (current (if from-end
                        (1- end2)
                        start2)))
      (do-abstract-sequence (x sequence1 (cond
                                           ((iterator-endp sequence2 current start2 end2 from-end) nil)
                                           ((= (- end2 start2) (- i start1)) nil)
                                           (from-end (- end1 i))
                                           (t i)))
          (i start1 end1 from-end)
        (when (iterator-endp sequence2 current start2 end2 from-end)
          (return (if from-end
                      (- end1 i)
                      (1- i))))
        (unless (funcall test (funcall key x) (funcall key (elt sequence2 current)))
          (return (if from-end
                      (- end1 i)
                      i)))
        (incf current (if from-end -1 1)))))
  (:method (sequence1 (sequence2 abstract-list) &key from-end (test #'eql) (start1 0) end1 (start2 0) end2 (key #'identity))
    (let ((end1 (and from-end
                     (or end1
                         (length sequence1))))
          (current (if from-end
                       (nreverse (subseq sequence2 start2 end2))
                       (drop start2 sequence2))))
      (do-abstract-sequence (x sequence1 (cond
                                           ((emptyp current) nil)
                                           ((and end2
                                                 (= (- end2 start2) (- i start1))) nil)
                                           (from-end (- end1 i))
                                           (t i))) (i start1 end1 from-end)
        (when (emptyp current)
          (return (if from-end
                      (- end1 i)
                      (1- i))))
        (unless (funcall test (funcall key x) (funcall key (first current)))
          (return (if from-end
                      (- end1 i)
                      i)))
        (setq current (rest current))))))


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
  #.(or (documentation 'cl:count-if 'function) "")
  (declare (ignore from-end start end key))
  (etypecase sequence
    (cl:sequence (apply #'cl:count-if pred sequence args))
    (abstract-sequence (apply #'abstract-count-if pred sequence args))))
(define-typecase-compiler-macro count-if (&whole form pred sequence &rest args)
  (typecase sequence
    (cl:sequence `(cl:count-if ,@(cdr form)))))

(defgeneric abstract-count-if (pred sequence &key from-end start end key)
  (:method (pred (sequence abstract-vector) &key from-end (start 0) end (key #'identity))
    (let ((count 0))
      (do-abstract-sequence (x sequence count) (i start end from-end)
        (when (funcall pred (funcall key x))
          (incf count)))))
  (:method (pred (sequence abstract-list) &key from-end (start 0) end (key #'identity))
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
  (:method ((sequence abstract-vector))
    (do ((i 0 (1+ i))
         (j (1- (abstract-length sequence)) (1- j)))
        ((>= i j) sequence)
      (psetf (abstract-elt sequence i) (abstract-elt sequence j)
             (abstract-elt sequence j) (abstract-elt sequence i))))
  (:method ((sequence abstract-list))
    (let ((results '()))
      (do-abstract-sequence (x sequence
                               (make-sequence-like sequence i
                                                   :initial-contents results))
          (i 0)
        (cl:push x results)))))

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
  (:method (new pred (sequence abstract-vector) &key from-end (start 0) end count (key #'identity))
    (let ((subst-count 0))
      (do-abstract-sequence (x sequence sequence) (i start end from-end)
        (when (eql count subst-count)
          (return sequence))
        (when (funcall pred (funcall key x))
          (incf subst-count)
          (setf (abstract-elt sequence i) new)))))
  (:method (new pred (sequence abstract-list) &key from-end (start 0) end count (key #'identity))
    (let ((subst-count 0))
      (cond
        ((and count
              (zerop count)) sequence)
        (from-end
         (let ((buf '()))
           (do-abstract-sequence (x sequence (make-sequence-like sequence i
                                                                 :initial-contents buf))
               (i start end t)
             (if (and (not (eql count subst-count))
                      (funcall pred (funcall key x)))
                 (progn
                   (incf subst-count)
                   (push new buf))
                 (push x buf)))))
        (t
         (when (funcall pred (funcall key (abstract-first sequence)))
           (incf subst-count)
           (setf (abstract-first sequence) new))
         (do-abstract-sequence ((x xs xxs) sequence sequence) (i start end from-end)
           (declare (ignore x))
           (when (eql count subst-count)
             (return sequence))
           (when (emptyp xs)
             (return sequence))
           (when (funcall pred (funcall key (abstract-first xs)))
             (incf subst-count)
             (setf (abstract-first xs) new)
             (setf (abstract-rest xxs) xs))))))))

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


;;
;; Function: split-sequence, split-sequence-if
;; Generic Function: abstract-split-sequence, abstract-split-sequence-if

(defun split-sequence (delimiter sequence &rest args &key start end from-end count remove-empty-subseqs test key)
  #.(or (documentation 'split-sequence:split-sequence 'function) "")
  (declare (ignore start end from-end count remove-empty-subseqs test key))
  (etypecase sequence
    (cl:sequence (apply #'split-sequence:split-sequence delimiter sequence args))
    (abstract-sequence (apply #'abstract-split-sequence delimiter sequence args))))
(define-typecase-compiler-macro split-sequence (&whole form delimiter sequence &rest args)
  (typecase sequence
    (cl:sequence `(split-sequence:split-sequence ,@(cdr form)))))

(defgeneric abstract-split-sequence (delimiter sequence &key start end from-end count remove-empty-subseqs test key)
  (:method (delimiter (sequence abstract-sequence) &rest args &key start end from-end count remove-empty-subseqs (test #'eql test-specified-p) (key #'identity))
    (declare (ignore start end from-end count remove-empty-subseqs))
    (when test-specified-p
      (setq args (delete-from-plist args :test)))
    (apply #'abstract-split-sequence-if
           (lambda (x)
             (funcall test (funcall key x) delimiter))
           sequence
           args)))

(defun split-sequence-if (pred sequence &rest args &key start end from-end count remove-empty-subseqs key)
  #.(or (documentation 'split-sequence:split-sequence-if 'function) "")
  (declare (ignore start end from-end count remove-empty-subseqs key))
  (etypecase sequence
    (cl:sequence (apply #'split-sequence:split-sequence-if pred sequence args))
    (abstract-sequence (apply #'abstract-split-sequence-if pred sequence args))))
(define-typecase-compiler-macro split-sequence-if (&whole form pred sequence &rest args)
  (typecase sequence
    (cl:sequence `(split-sequence:split-sequence-if ,@(cdr form)))))

(defgeneric abstract-split-sequence-if (pred sequence &key start end from-end count remove-empty-subseqs key)
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
  (:method ((sequence abstract-vector) &key from-end (test #'eql) (start 0) end (key #'identity))
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
              ((iterator-endp sequence j start end from-end)
               (setf (abstract-elt sequence pointer) x)
               (incf pointer step))
            (when (funcall test
                           x
                           (funcall key (abstract-elt sequence j)))
              (incf deleted-count)
              (return)))))))
  (:method ((sequence abstract-list) &key from-end (test #'eql) (start 0) end (key #'identity))
    (if from-end
        (let ((buf '())
              (length 0))
          (do-abstract-sequence (x sequence) (i start end nil)
            (incf length)
            (cl:push x buf))
          (setq buf (cl:nreverse (cl:delete-duplicates buf
                                                       :test test
                                                       :key key)))
          (replace sequence
                   buf
                   :start1 start
                   :end1 end)
          (if end
              (progn
                (setf (%nthrest (+ start length -1) sequence)
                      (%nthrest end sequence))
                sequence)
              (adjust-sequence sequence (+ start (cl:length buf)))))
        (flet ((find-dup (x seq &key (start 0) end)
                 (do-abstract-sequence (y seq) (i start end nil)
                   (when (funcall test (funcall key x) (funcall key y))
                     (return t)))))
          (let ((seq (%nthrest start sequence)))
            (loop while (find-dup (abstract-first seq) (abstract-rest seq)
                                  :start 0 :end (and end (1- end)))
                  do (setq seq (abstract-rest seq)))
            (do-abstract-sequence ((x xs xxs) seq) (i start end nil)
              (declare (ignore x))
              (when (emptyp xs)
                (return))
              (when (find-dup (abstract-first xs) (abstract-rest xs)
                              :start 0 :end (and end
                                                 (- end i 1)))
                (setf (abstract-rest xxs) (abstract-rest xs))
                (setq xs (abstract-rest xs))
                (return)))
            (cond
              ((zerop start) (setq sequence seq))
              ((= start 1)
               (setf (abstract-rest sequence) seq))
              (t (setf (%nthrest sequence start)
                       seq)))
            sequence)))))
