;;;; tests/main.lisp

(in-package #:com.danielkeogh.persistent-sequences-tests)

(def-suite all-tests
  :description "Main test suite for persistent-sequences")

(in-suite all-tests)

;;; Assertion macros

(defmacro s= (s1 s2)
  `(ps:sequence-equal ,s1 ,s2))

(defmacro s!= (s1 s2)
  `(not (ps:sequence-equal ,s1 ,s2)))

(defmacro start= (s1 s2)
  `(ps:sequence-starts-with ,s1 ,s2))

(defmacro start!= (s1 s2)
  `(not (ps:sequence-starts-with ,s1 ,s2)))

;;; Equality

(test sequence-equal
  (is (s= (ps:vec 1 2 3) (ps:list 1 2 3)))
  (is (s!= (ps:vec 1 2 3) (ps:vec 1 2)))
  (is (s!= (ps:vec 1 2) (ps:vec 1 2 3)))
  (is (s!= (ps:vec 1 2) (ps:vec)))
  (is (s!= (ps:vec) (ps:vec 1 2)))
  (is (s= (ps:vec) (ps:list))))

(test sequence-starts-with
  (is (start= (ps:vec 1 2 3) (ps:list 1 2 3)))
  (is (start= (ps:vec 1 2 3) (ps:list 1)))
  (is (start= (ps:vec 1 2 3) nil))
  (is (start!= (ps:vec 1 2 3) (ps:list 3 2))))

(test empty-p
  (is (ps:empty-p (ps:vec)))
  (is (ps:empty-p (ps:hashmap)))
  (is (ps:empty-p nil))
  (is (not (ps:empty-p (ps:vec 1))))
  (is (not (ps:empty-p (ps:list 1))))
  (is (not (ps:empty-p (ps:hashmap 1 1)))))

(test nonempty-p
  (is (not (ps:nonempty-p (ps:vec))))
  (is (not (ps:nonempty-p (ps:hashmap))))
  (is (not (ps:nonempty-p nil)))
  (is (ps:nonempty-p (ps:vec 1)))
  (is (ps:nonempty-p (ps:list 1)))
  (is (ps:nonempty-p (ps:hashmap 1 1))))

;;; Comparers

(test =
  (is (s= (ps:vec 1 2 3) (ps:sort (ps:vec 3 2 1) #'ps:<-comparer)))
  (is (s= (ps:vec 3 2 1) (ps:sort (ps:vec 1 2 3) #'ps:>-comparer))))

(test char=
  (is (s= (ps:vec #\a #\b #\c) (ps:sort (ps:vec #\c #\b #\a) #'ps:char<-comparer)))
  (is (s= (ps:vec #\c #\b #\a) (ps:sort (ps:vec #\a #\b #\c) #'ps:char>-comparer))))

(test char-equal
  (is (s= (ps:vec #\A #\b #\C) (ps:sort (ps:vec #\C #\b #\A) #'ps:char-lessp-comparer)))
  (is (s= (ps:vec #\C #\b #\A) (ps:sort (ps:vec #\A #\b #\C) #'ps:char-greaterp-comparer))))

(test string=
  (is (s= (ps:vec "a" "b" "c") (ps:sort (ps:vec "c" "b" "a") #'ps:string<-comparer)))
  (is (s= (ps:vec "c" "b" "a") (ps:sort (ps:vec "a" "b" "c") #'ps:string>-comparer))))

(test string-equal
  (is (s= (ps:vec "A" "b" "C") (ps:sort (ps:vec "C" "b" "A") #'ps:string-lessp-comparer)))
  (is (s= (ps:vec "C" "b" "A") (ps:sort (ps:vec "A" "b" "C") #'ps:string-greaterp-comparer))))

;;; Ordering

(test sort
  (is (s= (ps:vec 1 2 3) (ps:sort (ps:vec 3 2 1) #'ps:<-comparer)))
  (is (s= (ps:vec 1) (ps:sort (ps:vec 1)  #'ps:<-comparer)))
  (is (s= (ps:vec) (ps:sort (ps:vec) #'ps:<-comparer))))

(test reverse
  (is (s= (ps:vec 1 2 3) (ps:reverse (ps:vec 3 2 1))))
  (is (s= nil (ps:reverse nil))))

;;; Builders

(test assoc
  (is (ps:has-key (ps:assoc (ps:hashmap) 1 1) 1))
  (is (ps:has-key (ps:assoc (ps:treemap #'ps:<-comparer) 1 1) 1)))

(test conj
  (is (s= (ps:vec 1 2 3) (ps:conj (ps:vec 1 2) 3)))
  (is (s= (ps:vec 1 2 3) (ps:conj (ps:list 2 3) 1)))
  (is (s= (ps:vec 1 2 3) (ps:conj (ps:range 2 4) 1)))
  (is (s= (ps:vec 1) (ps:conj nil 1))))

;;; Growers

(test range
  (is (s= (ps:vec) (ps:range 0 0)))
  (is (s= (ps:vec 0 1 2 3) (ps:range 0 4))))

(test repeat
  (is (start= (ps:repeat 1) (ps:vec 1 1 1 1 1 1 1 1 1 1 1)))
  (is (s= (ps:repeat 1 2) (ps:vec 1 1))))

(test concat
  (is (s= (ps:vec 1 2 3 4 5 6) (ps:concat (ps:vec 1 2 3) (ps:vec 4 5 6))))
  (is (s= (ps:vec 1 2) (ps:concat (ps:vec 1) (ps:vec 2))))
  (is (s= (ps:vec) (ps:concat (ps:vec) (ps:vec))))
  (is (s= (ps:vec 1) (ps:concat (ps:vec 1) (ps:vec))))
  (is (s= (ps:vec 1) (ps:concat (ps:vec) (ps:vec 1)))))

(test cycle
  (is (start= (ps:cycle (ps:vec 1 2 3)) (ps:vec 1 2 3 1 2 3 1 2 3 1 2)))
  (is (s= (ps:cycle nil) nil)))

(test interleave
  (is (s= (ps:interleave (ps:range 0 3) (ps:range 0 1000)) (ps:vec 0 0 1 1 2 2)))
  (is (s= (ps:interleave nil (ps:range 0 1000)) nil)))

(test interpose
  (is (s= (ps:interpose (ps:range 0 3) :foo) (ps:vec 0 :foo 1 :foo 2)))
  (is (s= (ps:interpose nil :foo) nil)))

;;; Shrinkers

(test filter
  (is (s= (ps:filter (ps:range 0 10) #'evenp) (ps:vec 0 2 4 6 8))
      (is (s= (ps:filter (ps:range 0 10) (lambda ())) (ps:vec)))))

(test distinct
  (is (s= (ps:distinct (ps:vec 1 1 1 1 1 1)) (ps:vec 1)))
  (is (s= (ps:distinct nil) nil)))

(test take
  (is (s= (ps:take (ps:repeat 1) 5) (ps:vec 1 1 1 1 1)))
  (is (s= (ps:take nil 100) nil))
  (is (s= (ps:take (ps:range 0 10) 20) (ps:range 0 10))))

(test take-while
  (is (s= (ps:take-while (ps:range) (lambda (i) (<= i 5))) (ps:vec 0 1 2 3 4 5)))
  (is (s= (ps:take-while (ps:range) (lambda (i) (= 1 i))) (ps:vec)))
  (is (s= (ps:take-while (ps:vec) (lambda (i) (= 1 i))) (ps:vec))))

(test skip
  (is (s= (ps:skip (ps:range 0 10) 5) (ps:vec 5 6 7 8 9)))
  (is (s= (ps:skip (ps:range 0 10) 100) (ps:vec)))
  (is (s= (ps:skip (ps:vec) 100) (ps:vec))))

(test skip-while
  (is (s= (ps:skip-while (ps:range 0 10) (lambda (i) (< i 5))) (ps:vec 5 6 7 8 9)))
  (is (s= (ps:skip-while (ps:range 0 10) (lambda (i) (< i 100))) (ps:vec)))
  (is (s= (ps:skip-while (ps:range 0 5) (lambda (i) (= i 100))) (ps:vec 0 1 2 3 4)))
  (is (s= (ps:skip-while (ps:vec) (lambda (i) (= i 100))) (ps:vec))))

(test rest
  (is (s= (ps:rest nil) nil))
  (is (s= (ps:rest (ps:vec 1 2 3)) (ps:vec 2 3))))

(test but-last
  (is (s= (ps:but-last nil) nil))
  (is (s= (ps:but-last (ps:vec 1 2 3)) (ps:vec 1 2))))

(test subseq
  (is (s= (ps:subseq (ps:range) 5 10) (ps:vec 5 6 7 8 9)))
  (is (s= (ps:subseq (ps:range 0 10) 5) (ps:vec 5 6 7 8 9))))

;;; Searchers

(test contains-p
  (is (ps:contains-p (ps:range) 100))
  (is (not (ps:contains-p (ps:range 0 10) 100)))
  (is (ps:contains-p (ps:vec "abba" nil "scream" "test") "tEst" :test 'equalp))
  (is (null (ps:contains-p (ps:vec "abba" nil "scream" "test") "tEst" :test 'eq))))

;;; Aggregates

;; Length

(defmacro length-assert (collection length)
  `(= ,length (ps:length ,collection)))

(test vector-length
  (is (length-assert (ps:vec 1 2 3 4 5) 5))
  (is (length-assert (ps:vec) 0)))

(test list-length
  (is (length-assert (ps:list 1 2 3 4 5) 5))
  (is (length-assert (ps:list) 0)))

(test hashmap-length
  (is (length-assert (ps:hashmap 1 1 2 2 3 3 4 4 5 5) 5))
  (is (length-assert (ps:hashmap) 0)))

(test sequence-length
  (is (length-assert (ps:range 0 10) 10))
  (is (length-assert (ps:range 0 0) 0)))

;; Other Aggregates

(test reduce
  (is (= (ps:reduce (ps:range 0 4) '+) 6))
  (is (= (ps:reduce (ps:range 1 2) '+) 1))
  (is (= (ps:reduce nil '+) 0)))

;;; Looping

(test map
  (s= (ps:map (ps:range 0 5) (lambda (x) (+ x 10))) (ps:vec 10 11 12 13 14))
  (s= (ps:map nil (lambda (x) (+ x 10))) nil))

(test for
  (let ((sum 0))
    (ps:for (i (ps:range 0 4))
      (incf sum i))
    (is (= sum 6))))

;;; Nth

(test last
  (is (= 3 (ps:last (ps:vec 1 2 3))))
  (is (eq nil (ps:last nil))))

(test first
  (is (= 1 (ps:first (ps:vec 1 2 3))))
  (is (eq nil (ps:first nil))))

(test nth
  (is (= 1 (ps:nth (ps:range 0 10) 1)))
  (is (= 2 (ps:nth (ps:range 0 10) 2))))

;;; Key-Value Sequences

(test keys
  (is (s= (ps:sort (ps:keys (ps:hashmap 1 0 2 0 3 0 4 0)) #'ps:<-comparer) (ps:vec 1 2 3 4)))
  (is (s= (ps:keys (ps:treemap #'ps:<-comparer 1 0 2 0 3 0 4 0)) (ps:vec 1 2 3 4))))

(test vals
  (is (s= (ps:vals (ps:hashmap 1 0 2 0 3 0 4 0)) (ps:vec 0 0 0 0)))
  (is (s= (ps:vals (ps:treemap #'ps:<-comparer 1 0 2 0 3 0 4 0)) (ps:vec 0 0 0 0))))

(test has-key
  (is (ps:has-key (ps:hashmap 1 0) 1))
  (is (ps:has-key (ps:treemap #'ps:<-comparer 1 0) 1))
  (is (not (ps:has-key (ps:hashmap 1 0) 2)))
  (is (not (ps:has-key (ps:treemap #'ps:<-comparer 1 0) 2))))

(test val
  (is (= (ps:val (ps:hashmap 1 0) 1) 0))
  (is (= (ps:val (ps:treemap #'ps:<-comparer 1 0) 1) 0))
  (is (not (ps:val (ps:hashmap 1 0) 2)))
  (is (not (ps:val (ps:treemap #'ps:<-comparer 1 0) 2))))
