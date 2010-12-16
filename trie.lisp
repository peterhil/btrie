;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
;;;
;;; -------------------------------------------------------------------------
;;; Twist - P-TRIE - probability trie (with branch widths)
;;; -------------------------------------------------------------------------
;;;
;;; Copyright (c) 2010 Peter Hillerström, All Rights Reserved
;;; 
;;; Version:  0.2
;;; Initial Common Lisp version:  2010-08-01
;;; 
;;; -------------------------------------------------------------------------
;;; 
;;; Features:
;;; 
;;;   This trie implementation has an original idea of “branch width”
;;;   invented by Peter Hillerström on 14 of November 2008. Branch width
;;;   of a trie node tells how many branches go through that node.
;;;   Widths can be used to calculate probabilites for different suffixes.
;;; 
;;; Notes about this implementation:
;;; 
;;;   * P-trie is implemented recursively, so ‘trie’ can mean the whole 
;;;     tree or a single node on a trie.
;;;   * Keys can be sequences of any type.
;;;   * IMPORTANT: All functions are destructive, for efficiently handling 
;;;     large data sets. There will be non-destructive versions of functions.
;;; 
;;; About tries generally:
;;; 
;;;   Trie, or prefix tree, is an ordered tree data structure that is used 
;;;   to store an associative array where the keys are usually strings.
;;; 
;;;   Unlike a binary search tree, no node in the tree stores the whole key 
;;;   associated with that node instead, its position in the tree shows 
;;;   what key it is associated with.
;;;   
;;;   All the descendants of a node have a common prefix of the string 
;;;   associated with that node, and the root is associated with the empty 
;;;   string. Looking up a key of length m takes worst case O(m) time.
;;;   
;;;   More information about tries:
;;;   http://en.wikipedia.org/wiki/Trie
;;;

(in-package #:prodigy-logic)

;(setq *print-pretty* t)
(setq *print-circle* t)
(setq *print-level* 12)

#+sbcl
(defparameter +word-marker+ #\t) ; SBCL + swank causes problems with UTF-8
#-sbcl
(defparameter +word-marker+ #\u2022) ; • Bullet

(defparameter *debug* nil)
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

(defclass trie ()
  ((key
    :initform ""
    :reader key
    :initarg :key
    :type atom #|Equal to (not cons) |#
    ;:type character #|Could be :type char|# 
    :documentation "Can be any type for generality.")
   (width
    :initform 0
    :accessor width
    :initarg :width
    :type (integer 0 *))
   (branches
    :initform nil
    :accessor branches
    :initarg :branches
    :type list #|Could a vector, but it complicates things!|# ))
  (:documentation "Trie data structure, see package documentation for more info."))  

(defun test-trie (&optional (lst *banana*))
  "Simple utility function to build a test trie."
  (let ((r (make-trie)))
    (add-seqs r (mapcar #'(lambda (s) (string-downcase (string s))) lst))
    r))

(defun make-trie (&key (key "") (width 0) branches)
  "Utility function to make a trie instance."
  (make-instance 'trie :key key :width width :branches branches))

;;; Predicates

(defun leafp (node)
  "Predicate to tell if there are no branches for a node."
  (equal nil (branches node)))

(defun wordp (node)
  "Predicate to tell whether this node ends any words."
  (find-key node t))

(defun only-terminal-p (node)
  "This predicate tells if node has only terminal as a child."
  (and (= 1 (length (branches node)))
       (wordp node)))


;;; Retrieval

(defmethod obtain-seq ((trie trie) (seq sequence))
  "Get a sequence from trie by following the keys in the sequence."
  (when (zerop (length seq))
    (return-from obtain-seq trie))
  (let ((symbol (find-key trie (elt seq 0))))
    (when symbol
      (obtain-seq symbol (subseq seq 1)))))

(defmethod find-key ((trie trie) key)
  "Get a symbol matching key from trie's branches."
  (find key (branches trie) :test #'equal :key #'key))

(defmethod sym-interval ((trie trie) key)
  "Return interval limits for a symbol matching key.
  Nil if key not found."
  (when (find-key trie key)
    (let* ((node (find-key trie key))
           (low (sym-low trie key))
           (high (+ low (/ (width node)
                           (width trie)))))
        (values low high))))

(defmethod sym-low ((trie trie) key)
  "Return a cumulative lower limit for a symbol matching key.
  Nil if key not found."
  (when (find-key trie key)
        (/  (loop for b in (branches trie)
                  for k = (key b)
                  and w = (width b)
                  until (equal k key) summing w)
            (width trie))))
  ; Same cum-sum loop for assoc lists:
  ; (loop for (k . v) in al until (equal #\b key) summing v))

;;; Insertion

(defmethod add-seqs ((trie trie) (seqs list) &optional (count 1))
  (mapcar #'(lambda (seq) (add trie seq count)) seqs))

(defmethod add ((trie trie) (seq sequence) &optional (count 1))
  "Add a branch to the trie count times. Modifies trie in-place.
  If branch already exists, increase it’s width.
  A count below one is changed to one."
  (when (< count 0)
    (error (format nil "Negative count ~A." count)))
  (incf (width trie) count)
  (when (zerop (length seq))
    (add-key trie t count)
    (return-from add trie))
  (let ((symbol (add-key trie (elt seq 0) 0)))
    (add symbol (subseq seq 1) count)))

(defmethod add-key ((trie trie) key &optional (count 1))
  "Add a node to trie. If node exists, increases it’s width."
  (when (< count 0)
    (error (format nil "Negative count ~A." count)))
  (let ((node (find-key trie key)))
    (if node
      (incf (width node) count)
      (setq node (create-node trie key count)))
    node))

(defmethod create-node ((trie trie) key &optional (count 0))
  "Destructively adds node to trie"
  (car (push (make-trie :key key :width count) (branches trie))))


;;; Removal

(defmethod remove-node ((trie trie) key)
  (let ((node (find-key trie key)))
    (setf (branches trie)
          (remove node (branches trie)))
    node))

(defmethod remove-key ((trie trie) key &optional (count 1))
  "### Remove a node from trie. If node exists, decrease it’s width."
  ; 1. Find if key exist.
  (let ((node (find-key trie key)))
    (when (and node (>= (width node) count))
      ; 2. Decrease it's width when is less than or equal to count.
      (decf (width node) count)
      ; 3. If width is 0, remove node into new trie to be returned.
      (when (equal 0 (width node))
        (remove-node node key)))))

; (defun remove-seq (trie seq &optional (count 1))
;   "### Remove a sequence from trie"
;   (unless (obtain-seq trie seq)
;     (return-from remove-seq nil))
;   (decf (width trie) (max count 1))
;   (when (zerop (length seq))
;     (return-from remove-seq trie))
;   ; Use loop instead of recursion?
;   (let ((symbol (find-key trie (elt seq 0))))
;     (if symbol
;       (when (= count (width symbol))
;         ; Remove subtrie
;         (setf (branches r)
;             (delete #\b (branches r)
;             :test #'equal
;             :key (lambda (n) (key n))))
;         (remove-key trie (elt seq 0))
;         (return-from remove-seq nil))
;       (remove-seq symbol (subseq seq 1) count)))


;;; Probabilities

(defmethod trie-prob ((root trie) suffix)
  "Returns probability of suffix on given trie."
  (let ((node (obtain-seq root suffix)))
    (when node
      (/ (width node) (width root)))))


;;; Sorting
(defun sort-trie (trie predicate &rest args &key (key #'key) (stable nil))
  "Sort a trie recursively with a predicate function."
  (let ((root trie))
    (apply #'sort-trie-branch root predicate args)
    (unless (leafp trie)
      (loop as branch in (branches root) do
        (setf branch
              (apply #'sort-trie branch predicate args))))
    root))

(defun sort-trie-branch (trie predicate &key (key #'key) (stable nil))
  "Sort a trie node’s branches with a predicate function."
  (let ((branches (copy-list (branches trie)))
        (sort (if stable #'stable-sort #'sort)))
    (setf (branches trie) (funcall sort branches predicate :key key))
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

;; TODO Sliding window macro!

(defun print-words (trie &optional (stream t) (prefix ""))
  ; Could use more arguments: start end &key (with-count))
  "# Prints words from the trie, one per line.
  
  Options:
  * with-count: Prints word counts after tab when over one.
  
  TODO:
  * Use keyword arguments?
  * Implement start, end
  * Allow to specify separator insted of newline"
  
  (when (leafp trie)
    (let ((width (width trie)))
      ; Print the word and count if not in '(0 1).
      ; Logand changes 1 and 0 to 0 (and changes -1 to -2, 
      ; but that’s not the point).
      (format t "~A~:[~;~10t~d~]~%" prefix (/= 0 (logand -2 width)) width))
    (return-from print-words))
  (loop as branch in (branches trie) do
    (print-words branch stream
      (concatenate 'string prefix (format nil "~A" (key trie)))))
  (when (equal "" (key trie))
    (width trie)))

; (defmethod print-object ((trie trie) stream)
;   ; (when *print-pretty* (format stream "~&~v@T" 2))
;   (format stream "(~A ~d" (key trie) (width trie))
;   (unless (leafp trie)
;     (loop as branch in (branches trie) do
;       (write-char #\Space stream)
;       (write branch :stream stream)))
;   (format stream ")")
;   (when (leafp trie) (format stream "~%")))

(defun print-trie-simple (trie &optional (stream t) (depth 0) (indent 2))
  "## Traverse tries printing out nodes"
  (when indent (format stream "~&~v@T" (* depth indent)))
  (format stream "(~A ~d" (key trie) (width trie))
  (unless (leafp trie)
    (loop as branch in (branches trie) do
      (print-trie branch stream (+ 1 depth) indent)))
  (format stream ")"))

(defun print-list-trie (trie &optional (stream t) (compact t))
  "Pretty print the trie. Works when structure type is list."
  (pprint-logical-block (*standard-output* trie :prefix "(" :suffix ")")
    (let ((*print-miser-width* nil)   ; Miser mode disables pprint-indent!
          (key (pprint-pop)))
      ; Print key
      (cond
        ((equal t key)    (write-char +word-marker+))
        ((characterp key) (write-char key))
        (t                (write key)))
      (write-char #\Space)
      (pprint-indent :current 0)
      
      ; Print width
      (write (pprint-pop))
      
      ; Word ending
      (when (and compact (wordp trie))
        (write-char #\Space)
        (write-char +word-marker+))
      
      ; Branches
      (let ((branches (pprint-pop)))
        (when branches
            (loop as branch in branches do
              (unless (equal compact (car branch))    ; Else previous level
                (when *debug* (write :^))
                (write-char #\Space)                  ; Space before branches
                (unless (only-terminal-p trie)
                  (pprint-newline :mandatory))        ; Newline after leaf
                (pprint-indent :current 0)
                (pprint-trie branch stream compact)   ; Next level
                (when *debug* (write :*)))
                (when *debug* (write :$)))
        (pprint-exit-if-list-exhausted))))))

(defmethod print-object ((trie trie) stream)
  (print-trie-to-stream trie stream))

(defmethod print-trie-to-stream ((trie trie) stream &optional (depth 0) (compact t))
  "Pretty print the trie."
  (let ((*standard-output* stream)
        (key (key trie)))
    
    ; Indent and write opaening parens
    (format stream "~v@T" (* 2 depth))
    (write-char #\( #|Left Paren|# )
    
    ; Print key
    (cond
      ((equal t key)    (write-char +word-marker+))
      ((characterp key) (write-char key))
      (t                (write key)))
    (write-char #\Space)
    
    ; Print width
    (write (width trie))
    
    ; Word ending
    (when (and compact (wordp trie))
          (write-char #\Space)
          (write-char +word-marker+))
    
    ; Branches
    (loop as branch in (branches trie) do
      (unless (leafp branch)
        (format stream "~%") ; (write-char #\Newline)
        (print-trie-to-stream branch stream (+ 1 depth) compact))
      (write-char #\) #|Right Paren|#))))


(defmethod walker ((trie trie) (seq sequence) (fun function))
  "### Walk the trie with key perfoming function on found node. TODO? Almost same as obtain-seq!

    function walker(word, trie, method) {
        if (!word || !trie || !method) return null;
        var ch, c, l, i, prev;
        
        while (word.length > 0) {
            ch = word.charAt(0),
            c  = trie.children,
            l  = c.length,
            i  = 0;
            for (; i < l; ++i) {
                if (ch == c[i].stem)
                    break;
            }
            if (i == l)
                return null; // not found
            word = word.substring(1),
            prev = trie,
            trie = c[i];
        }
        return method(prev, i);
    }
")
