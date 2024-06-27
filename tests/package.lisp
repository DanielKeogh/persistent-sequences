;;;; tests/package.lisp

(defpackage #:com.danielkeogh.persistent-sequences-tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:ps #:com.danielkeogh.persistent-sequences))
  (:export #:run!
	   #:all-tests))
