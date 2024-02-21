# persistent-sequences

A Persistent/Immutable sequences implementation that builds upon the `persistent-vector`, `persisent-list` and `hashtrie` libaries, providing a large number of utility functions.

`persistent-sequences` is intended to be fast, thread-safe and composable.


## Usage

Given that `persistent-sequences` collides with several important functions in the `:common-lisp` namespace it is receommended that this library is used with a local nickname. For example, like this:

```lisp
(defpackage my-package
	(:use #:cl)
	(:local-nicknames (#:ps #:persistent-sequences)))
```

## Compatability

`persistent-sequences` builds upon other stand-alone peristent data-structure libraries, and can be used simultaneously with these, but it is intended to be used instead of any of these libraries as it provides a generic API for working with these datastructures as well as many utility functions that do not exist in these libraries.
