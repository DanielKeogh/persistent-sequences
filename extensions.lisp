;;;; extensions.lisp

(in-package #:persistent-sequences)

;;; Generic definitions

(defgeneric get-iterator (seq)
  (:documentation "Should return a (function () (values boolean T)) where the first return value indicates that there are items in the collection that have not been yielded, and the second value will be a different element of the collection each time the function is called."))

(defgeneric get-length (seq)
  (:documentation "Get the length of a sequence, either by counting or more specialised means."))

(defgeneric keys (key-value-set)
  (:documentation "Should return a sequence implementing 'get-iterator' containing the keys of a key-value set such as a tree or hashmap."))

(defgeneric vals (key-value-set)
  (:documentation "Should return a sequence implementing 'get-iterator' containing the values of a key-value set such as a tree or hashmap."))

;;; get-iterator

(defmethod get-iterator ((vector persistent-vector:persistent-vector))
  (persistent-vector::vec-make-iterator vector))

(defmethod get-iterator ((list persistent-list:persistent-list))
  (let ((remaining list)
	(cnt (persistent-list:length list)))
    (lambda ()
      (when (> cnt 0)
	(decf cnt)
	(let ((val (persistent-list:first remaining)))
	  (setf remaining (persistent-list:rest remaining))
	  (values t val))))))

(defmethod get-iterator ((seq persistent-sequence))
  (ps-get-iterator seq))

(defmethod get-iterator ((seq null))
  (lambda ()))

;;; get-length

(defmethod get-length ((vector persistent-vector:persistent-vector))
  (persistent-vector:length vector))

(defmethod get-length ((list persistent-list:persistent-list))
  (persistent-list:length list))

(defmethod get-length ((trie hashtrie:hashtrie))
  (hashtrie:length trie))

(defmethod get-length ((seq persistent-sequence))
  (let ((itr (ps-get-iterator seq)))
    (loop while (funcall itr)
	  count t)))

(defmethod get-length ((seq null))
  0)

;;; keys

(defmethod keys ((trie hashtrie:hashtrie))
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let ((itr (hashtrie::map-make-iterator trie)))
       (lambda ()
	 (multiple-value-bind (has-value key value) (funcall itr)
	   (declare (ignore value))
	   (values has-value key)))))))


;;; vals

(defmethod vals ((trie hashtrie:hashtrie))
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let ((itr (hashtrie::map-make-iterator trie)))
       (lambda ()
	 (multiple-value-bind (has-value key value) (funcall itr)
	   (declare (ignore key))
	   (values has-value value)))))))
