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

(in-package #:twist)

(setq *print-pretty* t)
(setq *print-circle* t)
(setq *print-level* 12)

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

(defstruct
  (trie 
    (:print-object bprint-trie))
    ; (:type list))
  "Trie data structure, see package documentation for more info."
  (key "" :read-only t)       ; Generic type -- could be :type char
  (width 0 :type integer)
  (branches nil :type list))  ; Could a vector, but it complicates things!

(defun test-trie (&optional (lst *banana*))
  "Simple utility function to build a test trie."
  (let ((r (make-trie)))
    (trie-add-seqs r (mapcar #'(lambda (s) (string-downcase (string s))) lst))
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

(defun obtain-seq (trie seq)
  "Get a sequence from trie by following the keys in the sequence."
  (when (zerop (length seq))
    (return-from obtain-seq trie))
  (let ((symbol (find-key trie (elt seq 0))))
    (when symbol
      (obtain-seq symbol (subseq seq 1)))))

(defun find-key (trie key)
  "Get a symbol matching key from trie's branches."
  (find key (trie-branches trie) :test #'equal :key #'trie-key))

(defun sym-interval (trie key)
  "Return interval limits for a symbol matching key.
  Nil if key not found."
  (when (find-key trie key)
    (let* ((node (find-key trie key))
           (low (sym-low trie key))
           (high (+ low (/ (trie-width node)
                           (trie-width trie)))))
        (values low high))))

(defun sym-low (trie key)
  "Return a cumulative lower limit for a symbol matching key.
  Nil if key not found."
  (when (find-key trie key)
        (/  (loop for b in (trie-branches trie)
                  for k = (trie-key b)
                  and w = (trie-width b)
                  until (equal k key) summing w)
            (trie-width trie))))
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
  (incf (trie-width trie) count)
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
      (incf (trie-width node) count)
      (setq node (create-node trie key count)))
    node))

(defun create-node (trie key &optional (count 0))
  "Destructively adds node to trie"
  (car (push (make-trie :key key :width count) (trie-branches trie))))


;;; Removal

(defun remove-node (trie key)
  (let ((node (find-key trie key)))
    (setf (trie-branches trie)
          (remove node (trie-branches trie)))
    node))

(defun remove-key (trie key &optional (count 1))
  "### Remove a node from trie. If node exists, decrease it’s width."
  ; 1. Find if key exist.
  (let ((node (find-key trie key)))
    (when (and node (>= (trie-width node) count))
      ; 2. Decrease it's width when is less than or equal to count.
      (decf (trie-width node) count)
      ; 3. If width is 0, remove node into new trie to be returned.
      (when (equal 0 (trie-width node))
        (remove-node node key)))))

; (defun remove-seq (trie seq &optional (count 1))
;   "### Remove a sequence from trie"
;   (unless (obtain-seq trie seq)
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
  (let ((node (obtain-seq root suffix)))
    (when node
      (/ (trie-width node) (trie-width root)))))


;;; Sorting

(defun sort-trie (trie predicate &key (key #'trie-key) (stable nil))
  "Sort a trie recursively with a predicate function."
  (let ((root trie))
    (sort-trie-branch root predicate :key key :stable stable)
    (unless (leafp trie)
      (loop as branch in (trie-branches root) do
        (setf branch
              (sort-trie branch predicate :key key :stable stable))))
    root))

(defun sort-trie-branch (trie predicate &key (key #'trie-key) (stable nil))
  "Sort a trie node’s branches with a predicate function."
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
    (let ((width (trie-width trie)))
      ; Print the word and count if not in '(0 1).
      ; Logand changes 1 and 0 to 0 (and changes -1 to -2, 
      ; but that’s not the point).
      (format t "~A~:[~;~10t~d~]~%" prefix (/= 0 (logand -2 width)) width))
    (return-from print-words))
  (loop as branch in (trie-branches trie) do
    (print-words branch stream
      (concatenate 'string prefix (format nil "~A" (trie-key trie)))))
  (when (equal "" (trie-key trie))
    (trie-width trie)))

; (defmethod print-object ((trie trie) stream)
;   ; (when *print-pretty* (format stream "~&~v@T" 2))
;   (format stream "(~A ~d" (trie-key trie) (trie-width trie))
;   (unless (leafp trie)
;     (loop as branch in (trie-branches trie) do
;       (write-char #\Space stream)
;       (write branch :stream stream)))
;   (format stream ")")
;   (when (leafp trie) (format stream "~%")))

(defun print-trie (trie &optional (stream t) (depth 0) (indent 2))
  "## Traverse tries printing out nodes"
  (when indent (format stream "~&~v@T" (* depth indent)))
  (format stream "(~A ~d" (trie-key trie) (trie-width trie))
  (unless (leafp trie)
    (loop as branch in (trie-branches trie) do
      (print-trie branch stream (+ 1 depth) indent)))
  (format stream ")"))

(defun pprint-trie (trie &optional (stream t) (compact t))
  "Pretty print the trie. Works when structure type is list."
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

(defun bprint-trie (trie &optional (stream t) (depth 0) (compact t))
  "## Pretty print the trie. Works with structure types."
  (let ((*standard-output* stream)
        (key (trie-key trie)))
    
    ; Indent and write opening parens
    (format stream "~v@T" (* 2 depth))
    (write-char #\( #|Left Paren|# )
    
    ; Print key
    (cond
      ((equal t key)    (write-char #\u2022))
      ((characterp key) (write-char key))
      (t                (write key)))
    (write-char #\Space)
    
    ; Print width
    (write (trie-width trie))
    
    ; Word ending
    (when (and compact (wordp trie))
          (write-char #\Space)
          (write-char #\u2022 #|Bullet|#))
    
    ; Branches
    (loop as branch in (trie-branches trie) do
      (unless (leafp branch)
        (format stream "~%") ; (write-char #\Newline)
        (bprint-trie branch stream (+ 1 depth) compact))
      (write-char #\) #|Right Paren|#))))



