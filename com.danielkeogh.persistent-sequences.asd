;;;; com.danielkeogh.persistent-sequences.asd

(asdf:defsystem #:com.danielkeogh.persistent-sequences
  :description "Persitent/Immutable data structures with a unifying sequence API."
  :author "Daniel Keogh"
  :license "Eclipse 1.0"
  :version "1.0.0"
  :serial t
  :depends-on (:hashtrie :persistent-list :persistent-tree-map :persistent-vector :alexandria)
  :components ((:file "package")
               (:file "persistent-sequences")
               (:file "extensions")
               (:file "api")))
