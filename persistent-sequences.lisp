;;;; persistent-sequences.lisp

(in-package #:com.danielkeogh.persistent-sequences)

(defstruct (persistent-sequence (:conc-name ps-))
  (iterator nil :type function))

(defun ps-get-iterator (seq)
  (funcall (ps-iterator seq)))

(defmethod print-object ((seq persistent-sequence) stream)
  (declare (type stream stream))
  (write-char #\( stream)
  (loop with itr = (ps-get-iterator seq)
        for (remaining val) = (multiple-value-list (funcall itr))
          then (cl:list next-remaining next-val)
        while remaining
        for (next-remaining next-val) = (multiple-value-list (funcall itr))
        do (prin1 val stream)
           (when next-remaining (write-char #\Space stream)))
  (write-char #\) stream))

(defmacro do-iterator ((&body get-next-body) (&body has-next-body))
  `(lambda ()
     (when (,@has-next-body)
       (values t (,@get-next-body)))))

(defun filter-iterator-fn (itr filter)
  (lambda ()
    (loop for (has-value value) = (multiple-value-list (funcall itr))
          while has-value
          when (funcall filter value) do (return (values has-value value)))))

(defun cl-vector->sequence (vector)
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let ((i 0)
           (max (cl:length vector)))
       (lambda ()
         (when (< i max)
           (let ((value (aref vector i)))
             (incf i)
             (values t value))))))))

(defun cl-list->sequence (lst)
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let ((remaining lst))
       (lambda ()
         (when remaining
           (let ((value (cl:first remaining)))
             (setf remaining (cl:rest remaining))
             (values t value))))))))

(defun sequence->cl-list (seq)
  (loop with itr = (get-iterator seq)
        for (has-value value) = (multiple-value-list (funcall itr))
        while has-value
        collect value))

(defun sequence->vector (seq)
  (loop with itr = (get-iterator seq)
        with vector = (make-array 0 :adjustable t :fill-pointer 0)
        for (has-value value) = (multiple-value-list (funcall itr))
        while has-value
        do (vector-push-extend value vector)
        finally (return vector)))

(defun %merge-sort (seq comparer-fn key-fn)
  (let ((comparer (if key-fn
                      (lambda (arg1 arg2)
                        (funcall comparer-fn
                                 (funcall key-fn arg1)
                                 (funcall key-fn arg2)))
                      comparer-fn)))
    (flet ((compare (arg1 arg2) (funcall comparer arg1 arg2)))
      (loop with source = (sequence->vector seq)
            with len = (cl:length source)
            with target = (make-array len)
            for offset = 1 then (+ offset offset)
            while (<= offset len) do
              (loop for block below len by (+ offset offset) do
                (loop for target-index from block below len
                      with i = block
                      with j = (+ block offset)
                      with max-i = j
                      with max-j = (min len (+ j offset))
                      do
                         (cond ((= i max-i)
                                (loop for s-x from j below max-j
                                      for t-x from target-index
                                      do (setf (aref target t-x) (aref source s-x)))
                                (return))
                               ((>= j max-j)
                                (loop for s-x from i below (min max-i len)
                                      for t-x from target-index
                                      do (setf (aref target t-x) (aref source s-x)))
                                (return))
                               (t
                                (let ((i-val (aref source i))
                                      (j-val (aref source j)))
                                  (if (<= (compare i-val j-val) 0)
                                      (progn
                                        (setf (aref target target-index) i-val)
                                        (incf i))
                                      (progn
                                        (setf (aref target target-index) j-val)
                                        (incf j))))))))
              (rotatef source target)
            finally (return source)))))
