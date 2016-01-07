(in-package :cl-user)
(defpackage cl21.core.cons
  (:use :cl)
  (:shadow :member
           :member-if
           :nth
           :nthcdr)
  (:import-from :cl21.core.sequence
                :abstract-sequence
                :abstract-list
                :drop-while
                :make-sequence-like
                :method-unimplemented-error

                :abstract-length
                :abstract-elt
                :abstract-first
                :abstract-second
                :abstract-third
                :abstract-fourth
                :abstract-fifth
                :abstract-sixth
                :abstract-seventh
                :abstract-eighth
                :abstract-ninth
                :abstract-tenth
                :abstract-rest
                :adjust-sequence
                :abstract-replace
                :abstract-copy-seq
                :abstract-subseq
                :abstract-take
                :abstract-drop
                :abstract-take-while
                :abstract-drop-while
                :abstract-last
                :abstract-find-if
                :abstract-position-if
                :abstract-search
                :abstract-delete-if
                :abstract-count-if
                :abstract-nreverse
                :abstract-nsubstitute-if
                :abstract-delete-duplicates
                :abstract-mismatch
                :abstract-fill
                :make-sequence-iterator
                :iterator-endp
                :iterator-next
                :iterator-pointer
                :with-sequence-iterator
                :do-abstract-sequence
                :take
                :drop)
  (:shadowing-import-from :cl21.core.sequence
                          :first
                          :rest
                          :length
                          :subseq
                          :nreverse
                          :replace
                          :copy-seq)
  (:shadowing-import-from :cl21.core.generic
                          :emptyp
                          :getf
                          :coerce)
  (:shadowing-import-from :cl21.core.array
                          :vector)
  (:import-from :cl21.internal.util
                :define-typecase-compiler-macro)
  (:import-from :alexandria
                :iota
                :remove-from-plist
                :delete-from-plist
                :ensure-list
                :ensure-cons
                :ensure-car
                #+nil :flatten
                :mappend
                :with-gensyms)
  (:export :list
           :null
           :cons
           :atom
           :consp
           :rplaca
           :rplacd
           :nth
           :nthcdr
           :car
           :cdr
           :caar
           :cadr
           :cdar
           :cddr
           :caaar
           :caadr
           :cadar
           :caddr
           :cdaar
           :cdadr
           :cddar
           :cdddr
           :caaaar
           :caaadr
           :caadar
           :caaddr
           :cadaar
           :cadadr
           :caddar
           :cadddr
           :cdaaar
           :cdaadr
           :cdadar
           :cdaddr
           :cddaar
           :cddadr
           :cdddar
           :cddddr
           :copy-tree
           :sublis
           :nsublis
           :subst
           :subst-if
           :nsubst
           :nsubst-if
           :tree-equal
           :copy-list
           :list*
           :listp
           :make-list
           :endp
           :null
           :nconc
           :nappend
           :revappend
           :nreconc
           :ldiff
           :tailp
           :mapcan
           :maplist
           :mapcon
           :acons
           :assoc
           :assoc-if
           :copy-alist
           :pairlis
           :rassoc
           :rassoc-if
           :get-properties
           :intersection
           :nintersection
           :adjoin
           :set-difference
           :nset-difference
           :set-exclusive-or
           :nset-exclusive-or
           :subsetp
           :union
           :nunion
           :list-length

           :iota
           :remove-from-plist
           :delete-from-plist
           :ensure-list
           :ensure-cons
           :ensure-car

           :list-push
           :list-pushnew
           :list-pop

           ;; Alexandria
           :mappend

           :maptree
           :1..
           :0..

           ;; Abstract List
           :member
           :member-if
           :abstract-member
           :abstract-member-if

           :flatten
           :abstract-flatten

           :last-cons
           :abstract-last-cons))
(in-package :cl21.core.cons)

(setf (symbol-function 'nappend) #'nconc)

(defmacro list-push (value place)
  `(cl:push ,value ,place))
(defmacro list-pushnew (value place &rest keys)
  `(cl:pushnew ,value ,place ,@keys))
(defmacro list-pop (place)
  `(cl:pop ,place))

(defun maptree (fn tree)
  (labels ((rec (tree)
             (etypecase tree
               (atom (funcall fn tree))
               (cons (cons (rec (car tree))
                           (if (cdr tree)
                               (rec (cdr tree))
                               nil))))))
    (if (null tree)
        nil
        (rec tree))))

(defun 1.. (n)
  (iota n :start 1))
(defun 0.. (n)
  (iota (1+ n)))


;;
;; Abstract List

(defmethod emptyp ((sequence abstract-list))
  (method-unimplemented-error 'emptyp sequence))

(defmethod getf ((place abstract-list) key &optional default)
  (let ((rest (nthcdr place key)))
    (if (emptyp rest)
        (values default nil)
        (values (abstract-first rest) t))))

(defmethod (setf getf) (newval (place abstract-list) key)
  (setf (abstract-elt place key) newval))

(defmethod abstract-length ((sequence abstract-list))
  (do-abstract-sequence (nil sequence i) (i)))

(defmethod abstract-elt ((sequence abstract-list) index)
  (abstract-first (drop index sequence)))

(defmethod (setf abstract-elt) (newval (sequence abstract-list) index)
  (setf (abstract-first (nthcdr sequence index)) newval))

(defmethod abstract-first ((sequence abstract-list))
  (method-unimplemented-error 'abstract-first sequence))

(defmethod (setf abstract-first) (newval (sequence abstract-list))
  (declare (ignore newval))
  (method-unimplemented-error '(setf abstract-first) sequence))

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
             for abstract-fun = (intern (format nil "~A-~A" :abstract function))
             append
             `((defmethod ,abstract-fun ((sequence abstract-list))
                 (dotimes (n ,n (abstract-first sequence))
                   (setq sequence (abstract-rest sequence))))
               (defmethod (setf ,abstract-fun) (newval (sequence abstract-list))
                 (dotimes (n ,n)
                   (setq sequence (abstract-rest sequence)))
                 (setf (abstract-first sequence) newval)))))

(defun nth (list n)
  (etypecase list
    (cl:list (cl:nth n list))
    (abstract-list (dotimes (i n (abstract-first list))
                     (setq list (abstract-rest list))))))
(define-typecase-compiler-macro nth (list n)
  (typecase list
    (cl:list `(cl:nth ,n ,list))))

(defun (setf nth) (newval list n)
  (etypecase list
    (cl:list (setf (cl:nth n list) newval))
    (abstract-list
     (dotimes (i n)
       (setq list (abstract-rest list)))
     (setf (abstract-first list) newval))))
(define-typecase-compiler-macro (setf nth) (newval list n)
  (typecase list
    (cl:list `(setf (cl:nth ,n ,list) ,newval))))

(defmethod abstract-rest ((sequence abstract-list))
  (method-unimplemented-error 'abstract-rest sequence))

(defmethod (setf abstract-rest) (newval (sequence abstract-list))
  (declare (ignore newval))
  (method-unimplemented-error '(setf abstract-rest) sequence))

(defun nthcdr (list n)
  (etypecase list
    (cl:list (cl:nthcdr n list))
    (abstract-list (dotimes (i n list)
                     (setq list (abstract-rest list))))))
(define-typecase-compiler-macro nthcdr (list n)
  (typecase list
    (cl:list `(cl:nthcdr ,n ,list))))

(defun (setf nthcdr) (newval list n)
  (etypecase list
    (cl:list (setf (cdr (cl:nthcdr (1- n) list)) newval))
    (abstract-list
     (when (> n 1)
       (dotimes (i (1- n))
         (setq list (abstract-rest list))))
     (setf (abstract-rest list) newval))))
(define-typecase-compiler-macro (setf nthcdr) (newval list n)
  (typecase list
    (cl:list `(setf (cl:nthcdr ,n ,list) ,newval))))

(defmethod adjust-sequence ((seq abstract-list) length &key initial-element (initial-contents nil icp))
  (if (eql length 0)
      (make-sequence-like seq 0)
      (let ((olength (abstract-length seq)))
        (cond
          ((eql length olength)
           (if icp
               (abstract-replace seq initial-contents)
               seq))
          ((< length olength)
           (setf (nthcdr seq length) (make-sequence-like seq 0))
           (if icp
               (abstract-replace seq initial-contents)
               seq))
          ((zerop olength)
           (let ((result (make-sequence-like seq length :initial-element initial-element)))
             (if icp
                 (abstract-replace result initial-contents)
                 result)))
          (t (setf (nthcdr seq olength)
                   (make-sequence-like seq
                                       (- length olength)
                                       :initial-element initial-element))
             (if icp
                 (abstract-replace seq initial-contents)
                 seq))))))

(defmethod coerce ((object abstract-list) type &key)
  (ecase type
    (list
     (let ((results '()))
       (do-abstract-sequence (x object (cl:nreverse results)) ()
         (cl:push x results))))
    ((vector simple-vector)
     (apply #'vector (coerce object 'list)))
    (string
     (cl:coerce (coerce object 'list) 'string))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defstruct (list-iterator (:constructor %make-list-iterator (sequence &key pointer limit step next-fn)))
    (pointer 0 :type integer)
    (limit nil :type (or integer null))
    (step +1 :type integer)
    (sequence nil :type (or cl:list abstract-list))
    (next-fn #'first :type function))

  (defmethod make-sequence-iterator ((sequence abstract-list) &key (start 0) end from-end)
    (if from-end
        (let ((end (or end
                       (abstract-length sequence))))
          (%make-list-iterator (abstract-nreverse (abstract-subseq sequence start end))
                               :pointer (1- end)
                               :step -1
                               :limit (1- start)))
        (%make-list-iterator (nthcdr sequence start)
                             :pointer start
                             :step +1
                             :limit end)))

  (defmethod make-sequence-iterator ((sequence cl:list) &key (start 0) end from-end)
    (if from-end
        (let ((seq (cl:nreverse (cl:subseq sequence start end))))
          (%make-list-iterator seq
                               :pointer (1- (or end (cl:length seq)))
                               :step -1
                               :limit (1- start)))
        (%make-list-iterator (cl:nthcdr start sequence)
                             :pointer start
                             :step +1
                             :limit end)))

  (defmethod iterator-pointer ((iterator list-iterator))
    (list-iterator-pointer iterator))

  (defmethod iterator-endp ((iterator list-iterator))
    (or (eql (list-iterator-limit iterator)
             (list-iterator-pointer iterator))
        (emptyp (list-iterator-sequence iterator))))

  (defmethod iterator-next ((iterator list-iterator))
    (prog1
        (funcall (list-iterator-next-fn iterator)
                 (list-iterator-sequence iterator))
      (setf (list-iterator-sequence iterator)
            (rest (list-iterator-sequence iterator)))
      (incf (list-iterator-pointer iterator)
            (list-iterator-step iterator))))

  (defun make-cons-iterator (sequence &key (start 0) end from-end)
    (if from-end
        (let ((end (or end
                       (length sequence)))
              (buf '()))
          (dotimes (i start)
            (setq sequence (rest sequence)))
          (do ((i start (1+ i)))
              ((= i end))
            (cl:push sequence buf)
            (setq sequence (rest sequence)))
          (%make-list-iterator buf
                               :pointer (1- end)
                               :step -1
                               :limit (1- start)))
        (%make-list-iterator (nthcdr sequence start)
                             :pointer start
                             :step +1
                             :limit end
                             :next-fn #'identity))))

(defmacro do-abstract-cons ((var sequence &rest result-form) (&optional (i (gensym "I")) (start 0) end from-end) &body body)
  (with-gensyms (iterator)
    `(let ((,iterator (make-cons-iterator ,sequence :start ,start :end ,end :from-end ,from-end)))
       (with-sequence-iterator (,var ,iterator ,@result-form) (,i)
         ,@body))))

(defmethod abstract-copy-seq ((sequence abstract-list))
  (cond
    ((emptyp sequence) (make-sequence-like sequence 0))
    ((emptyp (abstract-rest sequence))
     (make-sequence-like sequence 1
                         :initial-contents (list (abstract-first sequence))))
    (T (let ((new (make-sequence-like sequence 1)))
         (setf (abstract-first new) (abstract-first sequence)
               (abstract-rest new) (copy-seq (abstract-rest sequence)))
         new))))

(defmethod abstract-subseq ((sequence abstract-list) start &optional end)
  (let ((results '()))
    (do-abstract-sequence (x sequence (make-sequence-like sequence (- i start)
                                                          :initial-contents
                                                          (cl:nreverse results)))
        (i start end)
      (cl:push x results))))

(defmethod abstract-fill ((sequence abstract-list) item &key (start 0) end)
  (do-abstract-cons (x sequence sequence) (i start end)
    (setf (abstract-first x) item)))

(defmethod abstract-drop (n (sequence abstract-list))
  (copy-seq (nthcdr sequence n)))

(defmethod abstract-take-while (pred (sequence abstract-list))
  (let ((results '()))
    (flet ((final (length)
             (make-sequence-like sequence length
                                 :initial-contents (cl:nreverse results))))
      (do-abstract-sequence (x sequence (final i)) (i)
        (unless (funcall pred x)
          (return (final i)))
        (cl:push x results)))))

(defmethod abstract-drop-while (pred (sequence abstract-list))
  (do-abstract-cons (x sequence) (i 0)
    (unless (funcall pred (abstract-first x))
      (return x))))

(defmethod abstract-last ((sequence abstract-list))
  (do-abstract-cons (x sequence) ()
    (when (emptyp (abstract-rest x))
      (return (abstract-first x)))))

(defmethod abstract-search (sequence1 (sequence2 abstract-list) &key from-end (test #'eql) (start1 0) end1 (start2 0) end2 (key #'identity))
  (when (typep sequence1 'abstract-sequence)
    (setq sequence1 (coerce sequence1 'list)))
  (setq sequence1 (cl:subseq sequence1 start1 end1))
  (let ((length (cl:length sequence1))
        (first-el (funcall key (car sequence1))))
    (do-abstract-cons (x sequence2) (i start2 end2 from-end)
      (when (and (funcall test (funcall key (abstract-first x)) first-el)
                 (every (lambda (x y)
                          (funcall test
                                   (funcall key x)
                                   (funcall key y)))
                        (cdr sequence1)
                        (coerce (take (1- length) (abstract-rest x)) 'list)))
        (return i)))))

(defmethod abstract-delete-if (pred (sequence abstract-list) &key from-end (start 0) end count (key #'identity))
  (let ((removed-count 0))
    (unless from-end
      (loop while (and (not (eql removed-count count))
                       (funcall pred (funcall key (abstract-first sequence))))
            do (setq sequence (abstract-rest sequence))
               (incf removed-count)))
    (do-abstract-cons (x sequence sequence) (i start end from-end)
      (when (eql removed-count count)
        (return sequence))
      (let ((rest (abstract-rest x)))
        (when (and (not (emptyp rest))
                   (funcall pred (funcall key (abstract-first rest))))
          (incf removed-count)
          (setf (abstract-rest x)
                (abstract-rest rest)))))))

(defmethod abstract-mismatch (sequence1 (sequence2 abstract-list) &key from-end (test #'eql) (start1 0) end1 (start2 0) end2 (key #'identity))
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
                                         (t i)))
        (i start1 end1 from-end)
      (when (emptyp current)
        (return (if from-end
                    (1+ i)
                    (1- i))))
      (unless (funcall test (funcall key x) (funcall key (first current)))
        (return (if from-end
                    (- end1 i)
                    i)))
      (setq current (rest current)))))

(defmethod abstract-nreverse ((sequence abstract-list))
  (if (emptyp sequence)
      sequence
      (let ((current (make-sequence-like sequence 0)))
        (do ()
            ((emptyp sequence) current)
          (let ((next-rest (abstract-rest sequence)))
            (setf (abstract-rest sequence) current)
            (setq current sequence)
            (setq sequence next-rest))))))

(defmethod abstract-nsubstitute-if (new pred (sequence abstract-list) &key from-end (start 0) end count (key #'identity))
  (let ((subst-count 0))
    (if (and count
             (zerop count))
        sequence
        (do-abstract-cons (x sequence sequence) (i start end from-end)
          (when (eql count subst-count)
            (return sequence))
          (when (funcall pred (funcall key (abstract-first x)))
            (incf subst-count)
            (setf (abstract-first x) new))))))

(defmethod abstract-delete-duplicates ((sequence abstract-list) &key from-end (test #'eql) (start 0) end (key #'identity))
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
              (setf (nthcdr sequence (+ start length -1))
                    (nthcdr sequence end))
              sequence)
            (adjust-sequence sequence (+ start (cl:length buf)))))

      (flet ((find-dup (x seq &key (start 0) end)
               (do-abstract-sequence (y seq) (i start end nil)
                 (when (funcall test (funcall key x) (funcall key y))
                   (return t)))))
        (let ((seq (nthcdr sequence start)))
          (loop while (find-dup (abstract-first seq) (abstract-rest seq)
                                :start 0 :end (and end (1- end)))
                do (setq seq (abstract-rest seq)))
          (do-abstract-cons (x seq) (i start end nil)
            (let ((rest (abstract-rest x)))
              (when (emptyp rest)
                (return))
              (when (find-dup (abstract-first rest) (abstract-rest rest)
                              :start 0 :end (and end
                                                 (- end i 1)))
                (setf (abstract-rest x) (abstract-rest rest))
                (return))))
          (cond
            ((zerop start) (setq sequence seq))
            ((= start 1)
             (setf (abstract-rest sequence) seq))
            (t (setf (nthcdr start sequence)
                     seq)))
          sequence))))


;;
;; Function: member, member-if
;; Generic Function: abstract-member, abstract-member-if

(defun member (item list &rest args &key key test)
  (declare (ignore key test))
  (etypecase list
    (cl:list (apply #'cl:member item list args))
    (abstract-list (apply #'abstract-member item list args))))
(define-typecase-compiler-macro member (&whole form item list &rest args)
  (typecase list
    (cl:list `(cl:member ,@(cdr form)))))

(defgeneric abstract-member (item list &key key test)
  (:method (item (list abstract-list) &key (key #'identity) (test #'eql))
    (drop-while (lambda (x)
                  (not (funcall test (funcall key x) item)))
                list)))

(defun member-if (test list &rest args &key key)
  (declare (ignore key))
  (etypecase list
    (cl:list (apply #'cl:member-if test list args))
    (abstract-list (apply #'abstract-member-if test list args))))
(define-typecase-compiler-macro member-if (&whole form test list &rest args)
  (typecase list
    (cl:list `(cl:member-if ,@(cdr form)))))

(defgeneric abstract-member-if (test list &key key)
  (:method (test (list abstract-list) &key key)
    (drop-while (if key
                    (lambda (x)
                      (not (funcall test (funcall key x))))
                    (complement test))
                list)))


;;
;; Function: flatten
;; Generic Function: abstract-flatten

(defun flatten (tree)
  (typecase tree
    (cl:list (alexandria:flatten tree))
    (abstract-list (abstract-flatten tree))
    (otherwise (list tree))))

(defgeneric abstract-flatten (tree)
  (:method ((tree abstract-list))
    (let ((buf '())
          (length 0))
      (do-abstract-sequence (x tree) ()
        (incf length)
        (cl:push (flatten x) buf))
      (make-sequence-like tree length
                          :initial-contents (nreverse buf)))))


;;
;; Function: last-cons
;; Generic Function: abstract-last-cons

(defun last-cons (list &optional (n 1))
  (etypecase list
    (cl:list (cl:last list n))
    (abstract-list (abstract-last-cons list n))))

(defgeneric abstract-last-cons (list &optional n)
  (:method ((list abstract-list) &optional (n 1))
    (do ((current list (abstract-rest current)))
        ((emptyp (nthcdr current n)) current))))
