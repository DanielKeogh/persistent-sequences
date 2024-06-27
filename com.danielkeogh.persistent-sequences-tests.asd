;;;; com.danielkeogh.persistent-sequences-tests.asdf

(asdf:defsystem #:com.danielkeogh.persistent-sequences-tests
  :description "Tests for persistent-sequences"
  :author "Daniel Keogh"
  :license "Eclipse 2.0"
  :depends-on (:persistent-sequences :fiveam)
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "main")))))
