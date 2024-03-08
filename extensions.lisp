;;;; extensions.lisp

(in-package #:persistent-sequences)

;;; Generic definitions

(defgeneric get-iterator (seq)
  (:documentation "Should return a (function () (values boolean T)) where the first return value indicates that there are items in the collection that have not been yielded, and the second value will be a different element of the collection each time the function is called."))

(defgeneric get-length (seq)
  (:documentation "Get the length of a sequence, either by counting or more specialised means."))

(defgeneric get-keys (key-value-set)
  (:documentation "Should return a sequence implementing 'get-iterator' containing the keys of a key-value set such as a tree or hashmap."))

(defgeneric get-vals (key-value-set)
  (:documentation "Should return a sequence implementing 'get-iterator' containing the values of a key-value set such as a tree or hashmap."))

(defgeneric add-key-value (collection key value)
  (:documentation "Add a key and value to a key-value style collection."))

(defgeneric add-value (collection value)
  (:documentation "Add a value to a collection."))

(defgeneric get-val (collection key)
  (:documentation "Get the corresponding value of a key-value pair."))

(defgeneric get-key (collection key)
  (:documentation "Check if a key exists in a key-value pair."))

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

(defmethod get-length ((tree-map persistent-tree-map:persistent-tree-map))
  (persistent-tree-map:length tree-map))

(defmethod get-length ((seq persistent-sequence))
  (let ((itr (ps-get-iterator seq)))
    (loop while (funcall itr)
	  count t)))

(defmethod get-length ((seq null))
  0)

;;; keys

(defmethod get-keys ((trie hashtrie:hashtrie))
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let ((itr (hashtrie::map-make-iterator trie)))
       (lambda ()
	 (multiple-value-bind (has-value key value) (funcall itr)
	   (declare (ignore value))
	   (values has-value key)))))))

(defmethod get-keys ((tree-map persistent-tree-map:persistent-tree-map))
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let ((itr (persistent-tree-map::make-iterator tree-map)))
       (lambda ()
	 (multiple-value-bind (has-value key value)
	     (funcall itr)
	   (declare (ignore value))
	   (values has-value key)))))))

;;; vals

(defmethod get-vals ((trie hashtrie:hashtrie))
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let ((itr (hashtrie::map-make-iterator trie)))
       (lambda ()
	 (multiple-value-bind (has-value key value) (funcall itr)
	   (declare (ignore key))
	   (values has-value value)))))))

(defmethod get-vals ((tree-map persistent-tree-map:persistent-tree-map))
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let ((itr (persistent-tree-map::make-iterator tree-map)))
       (lambda ()
	 (multiple-value-bind (has-value key value)
	     (funcall itr)
	   (declare (ignore key))
	   (values has-value value)))))))

;;; add-key-value

(defmethod add-key-value ((trie hashtrie:hashtrie) key value)
  (hashtrie:add trie key value))

(defmethod add-key-value ((tree-map persistent-tree-map:persistent-tree-map) key value)
  (persistent-tree-map:add tree-map key value))

;;; add-value

(defmethod add-value ((vector persistent-vector:persistent-vector) value)
  (persistent-vector:append vector value))

(defmethod add-value ((list persistent-list:persistent-list) value)
  (persistent-list:cons value list))

(defmethod add-value ((seq persistent-sequence) value)
  (make-persistent-sequence
   :iterator
   (lambda ()
     (let ((itr (get-iterator seq))
	   (first-value-seen nil))
       (lambda ()
	 (if first-value-seen
	     (funcall itr)
	     (progn (setf first-value-seen t)
		    (values t value))))))))

(defmethod add-value ((seq null) value)
  (persistent-list:cons value nil))

;;; get-val

(defmethod get-val ((hashtrie hashtrie:hashtrie) key)
  (hashtrie:value hashtrie key))

(defmethod get-val ((tree-map persistent-tree-map:persistent-tree-map) key)
  (persistent-tree-map:value tree-map key))

;;; get-key

(defmethod get-key ((hashtrie hashtrie:hashtrie) key)
  (hashtrie:has-key hashtrie key))

(defmethod get-key ((tree-map persistent-tree-map:persistent-tree-map) key)
  (persistent-tree-map:has-key tree-map key))
