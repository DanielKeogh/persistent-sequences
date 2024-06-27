;;;; package.lisp

(defpackage #:com.danielkeogh.persistent-sequences
  (:local-nicknames (:vec #:persistent-vector)
		    (:tri #:hashtrie)
		    (:lst #:persistent-list)
		    (:ptm #:persistent-tree-map))
  (:use #:cl)
  (:shadow
   #:map
   #:length
   #:reduce
   #:rest
   #:last
   #:first
   #:list
   #:sort
   #:reverse
   #:nth
   #:assoc
   #:subseq)
  
  (:export
   ;; Constructors
   #:vec
   #:list
   #:hashmap
   #:treemap

   ;; Equality
   #:sequence-equal
   #:sequence-starts-with
   #:empty-p
   #:nonempty-p

   ;; Comparers
   #:<-comparer
   #:>-comparer
   #:char<-comparer
   #:char>-comparer
   #:char-lessp-comparer
   #:char-greaterp-comparer
   #:string<-comparer
   #:string>-comparer
   #:string-lessp-comparer
   #:string-greaterp-comparer

   ;; Ordering
   #:sort
   #:reverse

   ;; Builders
   #:assoc
   #:conj
   
   ;; Growers
   #:range
   #:repeat
   #:concat
   #:cycle
   #:interleave
   #:interpose

   ;; Shrinkers
   #:filter
   #:distinct
   #:take
   #:take-while
   #:skip
   #:skip-while
   #:rest
   #:but-last
   #:subseq

   ;; Searchers
   #:contains-p

   ;; Aggregates
   #:length
   #:reduce

   ;; Looping
   #:map
   #:for

   ;; Nth
   #:last
   #:first
   #:nth

   ;; Key-Value Sequence
   #:keys
   #:vals
   #:has-key
   #:val))
