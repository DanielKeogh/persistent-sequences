;;;; package.lisp

(defpackage #:persistent-sequences
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
   #:nth)
  
  (:export
   ;; Builders
   #:vec
   #:list
   #:hashtrie

   ;; Equality
   #:sequence-equal
   #:sequence-starts-with
   #:empty-p
   #:nonempty-p

   ;; Ordering
   #:sort
   #:reverse
   
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

   ;; Aggregates
   #:length
   #:reduce

   ;; Looping
   #:map
   #:for

   ;;Nth
   #:last
   #:first
   #:nth))
