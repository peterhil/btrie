;;; -*- Mode: Lisp -*-
;;; 
;;; P-TRIE - probability trie (with branch widths)
;;; 
;;; Author:
;;;     Peter Hillerström
;;; Version:
;;;     0.1
;;; Initial Common Lisp version:
;;;     2010-08-01
;;; 
;;; Features:
;;;     This trie implementation has an original idea of “branch width”
;;;     invented by Peter Hillerström on 14 of November 2008.
;;;     Branch width of a trie node tells how many branches go through that node.
;;;     Widths can be used to calculate probabilites for different suffixes.
;;; 
;;; Notes about this implementation:
;;;     * P-trie is implemented recursively, so ‘trie’ can mean the whole tree 
;;;       or a single node on a trie.
;;;     * Keys can be sequences of any type.
;;;     * IMPORTANT: All functions are destructive, for efficiently handling 
;;;       large data sets. There will be non-destructive versions of functions.
;;; 
;;; About tries generally:
;;;     Trie, or prefix tree, is an ordered tree data structure that is used to store 
;;;     an associative array where the keys are usually strings. Unlike a binary search 
;;;     tree, no node in the tree stores the whole key associated with that node; 
;;;     instead, its position in the tree shows what key it is associated with.
;;;     
;;;     All the descendants of a node have a common prefix of the string associated 
;;;     with that node, and the root is associated with the empty string.
;;;     Looking up a key of length m takes worst case O(m) time.
;;;     
;;;     More information about tries:
;;;     http://en.wikipedia.org/wiki/Trie

(defpackage :ptrie
    (:export
        ; Predicates
        :branchesp :leafp :wordp
        
        ; Init and slots
        :make-trie :trie-key :trie-width :trie-branches
        
        ; Insert, retrieve and remove (CRUD) operations
        :add-seq :add-seqs
        :find-key :find-seq
        
        ; Printing
        :pprint-trie :print-words
        
        ; Extras
        :trie-prob :test-trie)
    (:use :cl))

(in-package :ptrie)

(setq *print-pretty* t)
(setq *print-circle* t)
(setq *debug* nil)

;;; Initialization
;;;
;;; With struct, trie nodes occupy (without contents):
;;; Struct:             8 bits for each slot (24 bits)
;;; (with contents:     + 16 bits for 1-char key = 40 bits)
;;;
;;; Alternative implementation could use lists, arrays or objects:
;;; Linked cons cells:  3 * cons (8 bits) = 24 bits
;;; Class:              16 for class, 20 for vector = 36 bits

(defstruct (trie (:type list))
    "Trie data structure, see package documentation for more info."
    (key "" :read-only t)       ; Generic type -- could be :type char
    (width 0 :type integer)
    (branches nil :type list))  ; Could a vector for optimization, but complicates things!

(defun test-trie ()
    "Simple utility function to build a test trie."
    (let ((r (make-trie)))
        (add-seqs r (mapcar #'(lambda (s) (string-downcase (string s)))
                            '(blow boa blush foo bar baz)))
        (pprint-trie t r)
        r))


;;; Predicates

(defun wordp (trie)
    (find-key trie t))

(defun branchesp (trie)
    ; TODO account for (t 1 nil) nodes for word endings
    (car (trie-branches trie)))

(defun leafp (trie)
    (not (branchesp trie)))


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
;     "### Remove a node from trie. If node exists, decrease it’s width.
;     
;     1. Find if key exist.
;     2. Decrease it's width when is less than or equal to count.
;     3. If width is 0, remove node into new trie to be returned."
;     nil)

; (defun remove-key-broken (trie key)
;     "###"
;     (let ((node (find-key trie key))
;           (branches (trie-branches trie)))
;         (when node
;             (decf (trie-width node)) ; FIXME
;             (when (<= 0 trie-width node)
;                 (setf branches (remove-if #'(lambda (x) (equal x key)) branches))))))


; (defun remove-seq (trie seq &optional (count 1))
;     "### Remove a sequence from trie"
;     (unless (find-seq trie seq)
;         (return-from remove-seq nil))
;     (decf (trie-width trie) (max count 1))
;     (when (zerop (length seq))
;         (return-from remove-seq trie))
;     ; Use loop instead of recursion?
;     (let ((symbol (find-key trie (elt seq 0))))
;         (if symbol
;             (when (= count (trie-width symbol))
;                 ; Remove subtrie
;                 (setf (trie-branches r)
;                       (delete #\b (trie-branches r)
;                       :test #'equal
;                       :key (lambda (n) (trie-key n))))
;                 (remove-key trie (elt seq 0))
;                 (return-from remove-seq nil))
;             (remove-seq symbol (subseq seq 1) count)))


;;; Probabilities

(defun trie-prob (root suffix)
    "Returns probability of suffix on given trie."
    (let ((node-width (or (trie-width (find-seq root suffix)) 0)))
        (/ node-width (trie-width root))))


;;; Sorting

;; #|TODO|#


;;; Traversal & printing
;;;
;;; Example of trie representation, for trie ROOT -> a -> (n s):
;;; ("" 2 
;;;     (a 2 
;;;         (s 1 •) 
;;;         (n 1 •)))
;;; 
;;; Or with raw argument: 
;;; ("" 2 (a 2 (s 1 (T 1)) (n 1 (T 1))))

(defun print-words (trie &optional (prefix "") start end &key (with-count))
    "## Prints words from the trie. Prints word counts after tab when over one.
    TODO: Use keyword arguments."
    (when (leafp trie)
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
;     (when *print-pretty* (format stream "~&~v@T" 2))
;     (format stream "(~S ~d" (trie-key trie) (trie-width trie))
;     (when (branchesp trie)
;         (loop as branch in (trie-branches trie) do
;             (print branch)))
;     (format stream ")"))

(defun pprint-trie (*standard-output* trie &key (raw nil))
    "Pretty print the trie."
    (let ((*print-miser-width* nil))  ; Miser mode disables pprint-indent!
        (pprint-logical-block (*standard-output* trie :prefix "(" :suffix ")")
            ; Print key
            (if (characterp (first trie))
                (write-char (pprint-pop))
                (write (pprint-pop)))
            (write-char #\Space)
            ; Print width
            (pprint-indent :current 0)
            (write (pprint-pop))
            ; Branches
            (cond
                ((branchesp trie)
                    ; Print list of Branches
                    (write-char #\Space)
                    (when *debug* (write :+))
                    (loop as branch in (trie-branches trie) do
                        (when *debug* (write :~))
                        ; Word endings
                        (unless raw
                            (when (wordp trie)          ; FIXME Prints extra bullets
                                (write-char #\u2022))   ; Bullet
                            (when (leafp branch) (return-from pprint-trie)))
                        ; THIS makes the newline after leaf!
                        (pprint-newline :mandatory)
                        (pprint-indent :current 0)
                        ; Next level
                        (pprint-trie *standard-output* branch :raw raw)
                        ; Exit after leaf
                        (pprint-exit-if-list-exhausted)
                        (when *debug* (write :*)))
                    (when *debug* (write :@)))
                (t ; Is leaf
                    (when *debug* (write :%))
                    (pprint-exit-if-list-exhausted))))))

(defun print-trie (trie &optional (depth 0) (indent 2))
    "Traverse tries printing out nodes"
    (when indent (format t "~&~v@T" (* depth indent)))
    (format t "(~S ~d" (trie-key trie) (trie-width trie))
    (when (branchesp trie)
        (loop as branch in (trie-branches trie) do
            (print-trie branch (+ 1 depth) indent)))
    (format t ")"))



