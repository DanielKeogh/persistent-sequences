;;;; api.lisp

(in-package :com.danielkeogh.persistent-sequences)

;;; Constructors

(defun vec (&rest items)
  "Create a new persistent vector."
  (apply #'persistent-vector:vec items))

(defun list (&rest items)
  "Create a new persistent list."
  (apply #'persistent-list:list items))

(defun hashmap (&rest args)
  "Create a new persistent hashtrie."
  (apply #'hashtrie:make-hashtrie args))

(defun treemap (comparer-fn &rest pairs)
  "Create a new persistent treemap."
  (apply #'persistent-tree-map:make-tree-map (cons comparer-fn pairs)))

;;; Equality

(defun sequence-equal (seq1 seq2 &key (test 'eql))
  (loop with itr1 = (get-iterator seq1)
        with itr2 = (get-iterator seq2)
        for (has-value1 value1) = (multiple-value-list (funcall itr1))
        for (has-value2 value2) = (multiple-value-list (funcall itr2))
        while (or has-value1 has-value2)
        always (and has-value1
                    has-value2
                    (funcall test value1 value2))))

(defun sequence-starts-with (seq starts-with &key (test 'eql))
  (loop with itr1 = (get-iterator seq)
        with itr2 = (get-iterator starts-with)
        for (has-value1 value1) = (multiple-value-list (funcall itr1))
        for (has-value2 value2) = (multiple-value-list (funcall itr2))
        while has-value2
        always (and has-value1
                    has-value2
                    (funcall test value1 value2))))

(defun nonempty-p (seq)
  (typecase seq
    (persistent-sequence (funcall (get-iterator seq)))
    (t (< 0 (length seq)))))

(defun empty-p (seq)
  (not (nonempty-p seq)))

;;; Comparers

(defmacro comparer (arg1 arg2 eq-fn comparer-fn)
  `(cond ((,eq-fn ,arg1 ,arg2) 0)
         ((,comparer-fn ,arg1 ,arg2) -1)
         (t 1)))

(defun >-comparer (num1 num2)
  (declare (type number num1 num2))
  (comparer num1 num2 = >))

(defun <-comparer (num1 num2)
  (declare (type number num1 num2))
  (comparer num1 num2 = <))

(defun char<-comparer (char1 char2)
  (declare (type character char1 char2))
  (comparer char1 char2 char= char<))

(defun char>-comparer (char1 char2)
  (declare (type character char1 char2))
  (comparer char1 char2 char= char>))

(defun char-lessp-comparer (char1 char2)
  (declare (type character char1 char2))
  (comparer char1 char2 char-equal char-lessp))

(defun char-greaterp-comparer (char1 char2)
  (declare (type character char1 char2))
  (comparer char1 char2 char-equal char-greaterp))

(defun string<-comparer (string1 string2)
  (declare (type string string1 string2))
  (comparer string1 string2 string= string<))

(defun string>-comparer (string1 string2)
  (declare (type string string1 string2))
  (comparer string1 string2 string= string>))

(defun string-lessp-comparer (string1 string2)
  (declare (type string string1 string2))
  (comparer string1 string2 string-equal string-lessp))

(defun string-greaterp-comparer (string1 string2)
  (declare (type string string1 string2))
  (comparer string1 string2 string-equal string-greaterp))

;;; Ordering

(defun sort (seq comparer-fn &key key-fn)
  (cl-vector->sequence (%merge-sort seq comparer-fn key-fn)))

(defun reverse (seq)
  (cl-list->sequence (cl:reverse (sequence->cl-list seq))))

;;; Builders

(defun assoc (collection key value)
  (add-key-value collection key value))

(defun conj (collection value)
  (add-value collection value))

;;; Growers

(defun range (&optional (start 0) end)
  (make-persistent-sequence
   :iterator
   (if end
       (lambda ()
         (let ((i start))
           (lambda ()
             (unless (>= i end)
               (let ((v i))
                 (incf i)
                 (values t v))))))
       (lambda ()
         (let ((i start))
           (lambda ()
             (let ((v i))
               (incf i)
               (values t v))))))))

(defun repeat (value &optional count)
  (make-persistent-sequence
   :iterator
   (if count
       (lambda ()
         (let ((i 0))
           (lambda ()
             (when (< i count)
               (incf i)
               (values t value)))))
       (lambda ()
         (lambda ()
           (values t value))))))

(defun concat (&rest seqs)
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let ((remaining-seqs seqs)
           itr)
       (lambda ()
         (loop do
           (if itr
               (multiple-value-bind (has-value value) (funcall itr)
                 (if has-value
                     (return (values has-value value))
                     (setf itr nil)))
               (when remaining-seqs
                 (setf itr (get-iterator (car remaining-seqs))
                       remaining-seqs (cdr remaining-seqs))))
               while (or itr remaining-seqs)))))))

(defun cycle (seq)
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let ((itr (get-iterator seq)))
       (lambda ()
         (multiple-value-bind (has-value value) (funcall itr)
           (if has-value
               (values has-value value)
               (progn (setf itr (get-iterator seq))
                      (funcall itr)))))))))

(defun interleave (seq1 seq2)
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let ((itr1 (get-iterator seq1))
           (itr2 (get-iterator seq2))
           (s 0))
       (lambda ()
         (case s
           (0 (multiple-value-bind (has-value value) (funcall itr1)
                (if has-value
                    (progn (setf s 1)
                           (values has-value value))
                    (progn (setf s 2)
                           nil))))
           (1 (multiple-value-bind (has-value value) (funcall itr2)
                (if has-value
                    (progn
                      (setf s 0)
                      (values has-value value))
                    (progn (setf s 2)
                           nil))))
           (2 nil)))))))

(defun interpose (seq item)
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let ((itr (get-iterator seq))
           (first-item t)
           interpose-next
           has-next-val
           next-val)
       (labels ((set-interpose ()
                  (multiple-value-bind (has-val val) (funcall itr)
                    (when has-val
                      (setf interpose-next t))
                    (setf has-next-val has-val
                          next-val val))))
         (lambda ()
           (cond (interpose-next
                  (setf interpose-next nil)
                  (values t item))
                 (has-next-val
                  (let ((has-val has-next-val)
                        (val next-val))
                    (set-interpose)
                    (values has-val val)))
                 (first-item
                  (setf first-item nil)
                  (multiple-value-bind (has-value value) (funcall itr)
                    (when has-value
                      (set-interpose)
                      (values has-value value)))))))))))

;;; Shrinkers

(defun filter (seq fn)
  (make-persistent-sequence
   :iterator
   (lambda () (filter-iterator-fn (get-iterator seq) fn))))

(defun distinct (seq &key (test 'eql))
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let* ((visited (make-hash-table :test test)))
       (filter-iterator-fn (get-iterator seq) (lambda (a)
                                                (unless (gethash a visited)
                                                  (setf (gethash a visited) a)
                                                  t)))))))

(defun take (seq count)
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let ((i 0)
           (itr (get-iterator seq)))
       (lambda ()
         (when (< i count)
           (incf i)
           (funcall itr)))))))

(defun take-while (seq fn)
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let ((itr (get-iterator seq))
           (continue t))
       (lambda ()
         (when continue
           (multiple-value-bind (has-value value) (funcall itr)
             (if (and has-value (funcall fn value))
                 (values has-value value)
                 (setf continue nil)))))))))

(defun skip (seq n)
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let ((itr (get-iterator seq))
           (skip t))
       (if (= n 0)
           (lambda () nil)
           (lambda ()
             (if skip
                 (loop for (has-value value) = (multiple-value-list (funcall itr))
                       for counter from 0
                       when (or (not has-value) (>= counter n))
                         do (setf skip nil)
                            (return (values has-value value))
                       while skip)
                 (funcall itr))))))))

(defun skip-while (seq fn)
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let ((itr (get-iterator seq))
           (skip t))
       (lambda ()
         (if skip
             (loop for (has-value value) = (multiple-value-list (funcall itr))
                   when (or (not has-value) (not (funcall fn value)))
                     do (setf skip nil)
                        (return (values has-value value))
                   while skip)
             (funcall itr)))))))

(defun rest (seq)
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let ((itr (get-iterator seq)))
       (funcall itr)
       (lambda ()
         (funcall itr))))))

(defun but-last (seq)
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let ((itr (get-iterator seq))
           has-next
           next)
       (multiple-value-setq (has-next next) (funcall itr))
       (lambda ()
         (when has-next
           (multiple-value-bind (has-value value) (funcall itr)
             (when has-value
               (let ((return-has-value has-next)
                     (return-value next))
                 (setf has-next has-value
                       next value)
                 (values return-has-value return-value))))))))))

(defun subseq (seq start &optional end)
  (if end
      (take (skip seq start) (- end start))
      (skip seq start)))

;;; Searchers

(defun contains-p (seq item &key (test 'eq))
  (loop with itr = (get-iterator seq)
        for (has-val val) = (multiple-value-list (funcall itr))
        while has-val
          thereis (funcall test val item)))

;;; Aggregates

(defun length (seq)
  (get-length seq))

(defun reduce (seq fn)
  (let ((itr (get-iterator seq)))
    (multiple-value-bind (has-first-val first-val) (funcall itr)
      (if has-first-val
          (loop
            for aggr = first-val then (funcall fn aggr next)
            for (has-next next) = (multiple-value-list (funcall itr))
            while has-next
            finally (return aggr))
          (funcall fn)))))


;;; Looping

(defun map (seq fn)
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let ((itr (get-iterator seq)))
       (lambda ()
         (multiple-value-bind (has-value value) (funcall itr)
           (when has-value
             (values has-value (funcall fn value)))))))))

(defmacro for ((value seq) &body body)
  (alexandria:with-gensyms (itr has-value)
    `(loop with ,itr = (get-iterator ,seq)
           for (,has-value ,value) = (multiple-value-list (funcall ,itr))
           while ,has-value
           do (progn ,@body))))

;;; Nth

(defun first (seq)
  (nth-value 1 (funcall (get-iterator seq))))

(defun last (seq)
  (loop with itr = (get-iterator seq)
        for last = nil then value
        for (has-value value) = (multiple-value-list (funcall itr))
        unless has-value do (return last)))

(defun nth (seq n)
  (loop with itr = (get-iterator seq)
        for i from 1 to n
        unless (funcall itr)
          do (error "Seq doesnt have ~a elements" n)
        finally (return (nth-value 1 (funcall itr)))))

;;; Key-Value Sequences

(defun keys (collection)
  (get-keys collection))

(defun vals (collection)
  (get-vals collection))

(defun has-key (collection key)
  (get-key collection key))

(defun val (collection key)
  (get-val collection key))
