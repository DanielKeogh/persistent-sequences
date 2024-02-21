;;;; persistent-sequences.lisp

(in-package #:persistent-sequences)

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

;; (defmacro iterator-with-yield ((yield) &body body)
;;   (let (tags)
;;     (labels ((walk-tree (tree)
;; 	       (loop for node in tree
;; 		     collect
;; 		     (cond ((listp node)
;; 			    (if (eq (car node) yield)
;; 				()
;; 				()
				
;; 				)
;; 			    )

;; 			   ) if (listp node)
;; 		     collect (walk-tree node)
;; 		     else
;; 		       collect node)))))
;;   (tagbody )

;;   )
