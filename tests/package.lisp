;;;; tests/package.lisp

(defpackage #:persistent-sequences-tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:ps #:persistent-sequences))
  (:export #:run!
	   #:all-tests))
