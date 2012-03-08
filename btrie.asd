(in-package #:cl-user)
(defpackage #:nu.composed.btrie.system
  (:use #:cl #:asdf))
(in-package #:nu.composed.btrie.system)

(defsystem btrie
  :author "Peter Hillerström <peter.hillerstrom@gmail.com>"
  :license "Simplified BSD license."
  :version "0.2.1"
  :description "Branch trie - a generic trie implementation with branch widths.

* Implementation is generic: keys can be of sequences of any type.
* Branch width of a trie node tells how many branches go through that node and
  can be used to calculate probabilites for different suffixes."
  :depends-on (:arnesi :split-sequence :lift)
  :serial t
  :components ((:file "btrie")))

(defsystem btrie-tests
  :depends-on (:btrie :lift :metabang-bind)
  :serial t
  :components ((:file "tests")))