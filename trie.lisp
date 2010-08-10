;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ptrie -*-
;;;
;;; ------------------------------------------------------------------------------------
;;; P-TRIE - probability trie (with branch widths)
;;; ------------------------------------------------------------------------------------
;;;
;;; Author:
;;;   Peter Hillerström
;;; Version:
;;;   0.1
;;; Initial Common Lisp version:
;;;   2010-08-01
;;; 
;;; Features:
;;;   This trie implementation has an original idea of “branch width”
;;;   invented by Peter Hillerström on 14 of November 2008.
;;;   Branch width of a trie node tells how many branches go through that node.
;;;   Widths can be used to calculate probabilites for different suffixes.
;;; 
;;; Notes about this implementation:
;;;   * P-trie is implemented recursively, so ‘trie’ can mean the whole tree 
;;;     or a single node on a trie.
;;;   * Keys can be sequences of any type.
;;;   * IMPORTANT: All functions are destructive, for efficiently handling 
;;;     large data sets. There will be non-destructive versions of functions.
;;; 
;;; About tries generally:
;;;   Trie, or prefix tree, is an ordered tree data structure that is used to store 
;;;   an associative array where the keys are usually strings. Unlike a binary search 
;;;   tree, no node in the tree stores the whole key associated with that node;;; 
;;;   instead, its position in the tree shows what key it is associated with.
;;;   
;;;   All the descendants of a node have a common prefix of the string associated 
;;;   with that node, and the root is associated with the empty string.
;;;   Looking up a key of length m takes worst case O(m) time.
;;;   
;;;   More information about tries:
;;;   http://en.wikipedia.org/wiki/Trie
;;;
;;; ------------------------------------------------------------------------------------

(defpackage :ptrie
  (:export
    ; Predicates
    :leafp :only-terminal-p :wordp
    
    ; Init and slots
    :make-trie :trie-key :trie-width :trie-branches
    
    ; Insert, retrieve and remove (CRUD) operations
    :add-seq :add-seqs
    :find-key :find-seq
    
    ; Sorting
    :sort-trie :sort-trie-branch
    
    ; Printing
    :pprint-trie :print-words
    
    ; Extras
    :trie-prob :test-trie
    
    *print-pretty* *print-circle*)
  (:use :cl))

(in-package :ptrie)

(setq *print-pretty* t)
(setq *print-circle* t)
(setq *debug* nil)

(defparameter *boa* '(blow boa blush foo bar baz))
(defparameter *banana* '(banana are an bare bane as c bar ar ban b barn))

;;; Initialization
;;;
;;; With struct, trie nodes occupy (without contents):
;;; Struct:       8 bits for each slot (24 bits)
;;; (with contents:   + 16 bits for 1-char key = 40 bits)
;;;
;;; Alternative implementation could use lists, arrays or objects:
;;; Linked cons cells:  3 * cons (8 bits) = 24 bits
;;; Class:        16 for class, 20 for vector = 36 bits

(defstruct (trie (:type list))
  "Trie data structure, see package documentation for more info."
  (key "" :read-only t)     ; Generic type -- could be :type char
  (width 0 :type integer)
  (branches nil :type list))  ; Could a vector for optimization, but complicates things!

(defun test-trie (&optional (lst *banana*))
  "Simple utility function to build a test trie."
  (let ((r (make-trie)))
    (add-seqs r (mapcar #'(lambda (s) (string-downcase (string s))) lst))
    (pprint-trie t r)
    r))


;;; Predicates

(defun leafp (node)
  "Predicate to tell if there are no branches for a node."
  (equal nil (trie-branches node)))

(defun wordp (node)
  "Predicate to tell whether this node ends any words."
  (find-key node t))

(defun only-terminal-p (node)
  "This predicate tells if node has only terminal as a child."
  (and (= 1 (length (trie-branches node)))
       (wordp node)))


;;; Retrieval

(defun find-seq (trie seq)
  "Find a sequence from trie"
  (when (zerop (length seq))
    (return-from find-seq trie))
  (let ((symbol (find-key trie (elt seq 0))))
    (when symbol
      (find-seq symbol (subseq seq 1)))))

(defun find-key (trie key)
  "Return a symbol matching key from trie's branches."
  (find key (trie-branches trie) :test #'equal :key (lambda (n) (trie-key n))))


;;; Insertion

(defun add-seqs (trie seqs)
  (mapcar #'(lambda (seq) (add-seq trie seq)) seqs))

(defun add-seq (trie seq &optional (count 1))
  "Add a branch to the trie count times. If branch already exists, increase it’s width.
  A count below one is changed to one. Modifies trie in-place."
  (when (< count 0)
    (error (format nil "Negative count ~A." count)))
  (incf (trie-width trie) count)
  (when (zerop (length seq))
    (add-key trie t count)
    (return-from add-seq trie))
  (let ((symbol (add-key trie (elt seq 0) 0)))
    (add-seq symbol (subseq seq 1) count)))

(defun add-key (trie key &optional (count 1))
  "Add a node to trie. If node exists, increases it’s width."
  (when (< count 0)
    (error (format nil "Negative count ~A." count)))
  (let ((node (find-key trie key)))
    (if node
      (incf (trie-width node) count)
      (setq node (create-node trie key count)))
    node))

(defun create-node (trie key &optional (count 0))
  "Destructively adds node to trie"
  (car (push (make-trie :key key :width count) (trie-branches trie))))


;;; Removal

; (defun remove-key (trie key &optional (count 1))
;   "### Remove a node from trie. If node exists, decrease it’s width.
;   
;   1. Find if key exist.
;   2. Decrease it's width when is less than or equal to count.
;   3. If width is 0, remove node into new trie to be returned."
;   nil)

; (defun remove-key-broken (trie key)
;   "###"
;   (let ((node (find-key trie key))
;       (branches (trie-branches trie)))
;     (when node
;       (decf (trie-width node)) ; FIXME
;       (when (<= 0 trie-width node)
;         (setf branches (remove-if #'(lambda (x) (equal x key)) branches))))))


; (defun remove-seq (trie seq &optional (count 1))
;   "### Remove a sequence from trie"
;   (unless (find-seq trie seq)
;     (return-from remove-seq nil))
;   (decf (trie-width trie) (max count 1))
;   (when (zerop (length seq))
;     (return-from remove-seq trie))
;   ; Use loop instead of recursion?
;   (let ((symbol (find-key trie (elt seq 0))))
;     (if symbol
;       (when (= count (trie-width symbol))
;         ; Remove subtrie
;         (setf (trie-branches r)
;             (delete #\b (trie-branches r)
;             :test #'equal
;             :key (lambda (n) (trie-key n))))
;         (remove-key trie (elt seq 0))
;         (return-from remove-seq nil))
;       (remove-seq symbol (subseq seq 1) count)))


;;; Probabilities

(defun trie-prob (root suffix)
  "Returns probability of suffix on given trie."
  (let ((node-width (or (trie-width (find-seq root suffix)) 0)))
    (/ node-width (trie-width root))))


;;; Sorting

(defun sort-trie (trie predicate &key (key #'trie-key) (stable nil))
  "Sort a trie recursively with a predicate function suitable for sorting."
  (let ((root trie))
    (sort-trie-branch root predicate :key key :stable stable)
    (unless (leafp trie)
      (loop as branch in (trie-branches root) do
        (setf branch
              (sort-trie branch predicate :key key :stable stable))))
    root))

(defun sort-trie-branch (trie predicate &key (key #'trie-key) (stable nil))
  "Sort a trie node’s branches with a predicate function suitable for sorting."
  (let ((branches (copy-list (trie-branches trie)))
        (sort (if stable #'stable-sort #'sort)))
    (setf (trie-branches trie)
          (funcall sort branches predicate :key key))
    trie))


;;; Traversal & printing
;;;
;;; Example of trie representation, for trie ROOT -> a -> (n s):
;;; ("" 2 
;;;   (a 2 
;;;     (s 1 •) 
;;;     (n 1 •)))
;;; 
;;; Or with raw argument: 
;;; ("" 2 (a 2 (s 1 (T 1)) (n 1 (T 1))))

(defun print-trie (trie &optional (depth 0) (indent 2))
  "Traverse tries printing out nodes"
  (when indent (format t "~&~v@T" (* depth indent)))
  (format t "(~S ~d" (trie-key trie) (trie-width trie))
  (unless (leafp trie)
    (loop as branch in (trie-branches trie) do
      (print-trie branch (+ 1 depth) indent)))
  (format t ")"))

(defun print-words (trie &optional (prefix "") start end &key (with-count))
  "## Prints words from the trie, one per line.
  
  Options:
  * with-count: Prints word counts after tab when over one.
  
  TODO:
  * Use keyword arguments?
  * Implement start, end
  * Allow to specify separator insted of newline
  "
  (when (only-terminal-p trie)
    (let ((width (trie-width trie)))
      ; Print the word and count if not in '(0 1).
      ; Logand changes 1 and 0 to 0 (and changes -1 to -2, but that’s not the point).
      (format t "~A~:[~;~10t~d~]~%" prefix (/= 0 (logand -2 width)) width))
    (return-from print-words))
  (loop as branch in (trie-branches trie) do
    (print-words branch
      (concatenate 'string prefix (format nil "~A" (trie-key trie)))))
  (when (equal "" (trie-key trie))
    (trie-width trie)))

; (defmethod print-object ((object trie) stream)
;   (when *print-pretty* (format stream "~&~v@T" 2))
;   (format stream "(~S ~d" (trie-key trie) (trie-width trie))
;   (unless (leafp trie)
;     (loop as branch in (trie-branches trie) do
;       (print branch)))
;   (format stream ")"))

(defun pprint-trie (*standard-output* trie &key (compact t))
  "Pretty print the trie."
  (pprint-logical-block (*standard-output* trie :prefix "(" :suffix ")")
    (let ((*print-miser-width* nil)   ; Miser mode disables pprint-indent!
          (key (pprint-pop)))
      ; Print key
      (cond
        ((equal t key)    (write-char #\u2022))
        ((characterp key) (write-char key))
        (t                (write key)))
      (write-char #\Space)
      (pprint-indent :current 0)
      
      ; Print width
      (write (pprint-pop))
      
      ; Word ending
      (when (and compact (wordp trie))
        (write-char #\Space)
        (write-char #\u2022 #|Bullet|#))
      
      ; Branches
      (let ((branches (pprint-pop)))
        (when branches
            (loop as branch in branches do
              (unless (equal compact (car branch))    ; Return to previous level on leaf
                (when *debug* (write :^))
                (write-char #\Space)                  ; Space before branches
                (unless (only-terminal-p trie)
                  (pprint-newline :mandatory))        ; Newline after leaf
                (pprint-indent :current 0)
                ; (pprint-exit-if-list-exhausted)
                (pprint-trie *standard-output* branch :compact compact)   ; Next level
                (when *debug* (write :*)))
                (when *debug* (write :$)))
        (pprint-exit-if-list-exhausted))))))







