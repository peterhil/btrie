;;; -*- coding: utf-8; mode: Lisp; syntax: ANSI-Common-Lisp -*-
;;;
;;; -------------------------------------------------------------------------
;;; Branch trie - a generic trie implementation with branch widths
;;; -------------------------------------------------------------------------
;;;
;;; Copyright (c) 2010-2012 Peter Hillerström, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(in-package #:cl-user)

(defpackage #:nu.composed.btrie
  (:documentation "Branch trie – an implementation of tries with branch widths.")
  (:nicknames :btrie)
  (:use :cl :arnesi :lift)
  (:export
   #:+word-marker+
   #:add
   #:add-seqs
   #:add-seqs-as-keys
   #:add-subseqs
   #:branches
   #:find-key
   #:key
   #:leafp
   #:make-leaf
   #:make-node
   #:make-trie
   #:make-word-trie
   #:nodes-equalp
   #:obtain-seq
   #:only-terminal-p
   #:print-words
   #:sort-trie
   #:sort-trie-branch
   #:test-trie
   #:traverse
   #:trie
   #:trie-branches
   #:trie-key
   #:trie-prob
   #:trie-width
   #:width
   #:wordp
   ))

(in-package #:nu.composed.btrie)

(setq *print-pretty* t)
(setq *print-circle* nil)
(setq *print-level* 12)

(defparameter +word-marker+ #\.) ; SBCL + swank causes problems with UTF-8 with bullets
(defparameter *debug* nil)


;;; Initialization
;;
;; With struct, trie nodes occupy (without contents):
;; Struct:              8 bits for each slot (24 bits)
;; (with contents:   + 16 bits for 1-char key = 40 bits)
;;
;; Alternative implementation could use lists, arrays or objects:
;; Linked cons cells:       3 * cons (8 bits) = 24 bits
;; Class:         16 for class, 20 for vector = 36 bits

(defclass trie ()
  ((key
    :type atom ; equal to (not cons)
    :initform ""
    :reader key
    :initarg :key
    :documentation "Can be any type for generality.")
   (width
    :type (integer 0 *)
    :initform 0
    :accessor width
    :initarg :width)
   (branches
    :type list
    :initform nil
    :accessor branches
    :initarg :branches))
  (:documentation "Trie data structure, see package documentation for more info."))


(defun make-node (&key (key "") (width 0) (branches nil) (leaf nil))
  "Utility function to make a trie instance."
  (if leaf
      (make-instance 'trie :key key :width width :branches '(nil))
      (make-instance 'trie :key key :width width :branches branches)))

(defun make-leaf (&key (width 1))
  (make-node :key t :width width))

(defun make-trie-with-fn (&optional (fn #'add-seqs) (seqs nil))
  "Simple utility function to build a trie from a sequence."
  (let ((r (make-node)))
    (funcall fn r (map 'list #'(lambda (s) (string-downcase (string s))) seqs))
    r))

(defun make-trie (&optional (seqs nil))
  "Make a trie with letters as keys."
  (make-trie-with-fn #'add-seqs seqs))

(defun make-word-trie (seqs)
  "Make a test trie with words as keys."
  (let ((r (make-node)))
    (add-seqs-as-keys r seqs)
    r))


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

(defun nodes-equalp (a b)
  (and
   (equal (key a) (key b))
   (equal (width a) (width b))
   (every #'nodes-equalp (branches a) (branches b))))


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


;;; Insertion

(defun subseqs (seq length)
  (loop for start from 0 to (- (length seq) length)
     collect (subseq seq start (+ start length))))

(defun interleave (seq num-parts)
  (cond ((zerop num-parts) nil)
	((> num-parts (length seq)) (warn "Can't interleave a sequence ~a into ~d parts" seq num-parts))
	(t (cut-sequence seq (1- num-parts)))))

(defun cut-sequence (seq times)
  (cond ((zerop times) seq)
	((<= 1 times (1- (length seq)))
	 (let ((sub-length (- (length seq) times)))
	   (arnesi:map-range (lambda (start) (subseq seq start (+ start sub-length))) 0 times)))
	(t (warn "Can't cut a sequence ~a ~d times" seq times))))

(defmethod add-seqs ((trie trie) (seqs list) &optional (count 1))
  (map 'list
       (lambda (seq) (add trie seq count))
       seqs))

(defmethod add-seqs-as-keys ((trie trie) (seqs sequence) &optional (count 1))
  (map (type-of seqs) (lambda (seq) (add trie seq count)) seqs))

(defmethod add-subseqs ((trie trie) (seq sequence) (len integer))
  (mapcar (lambda (s) (add trie s)) (subseqs seq len)))

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
  (car (push (make-node :key key :width count) (branches trie))))


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


;;; Probabilities

(defmethod trie-prob ((root trie) suffix)
  "Returns probability of suffix on given trie."
  (let ((node (obtain-seq root suffix)))
    (when node
      (/ (width node) (width root)))))


;;; Sorting

(defun sort-trie (trie predicate &rest args)
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
;;
;; Example of trie representation, for words '(as an) ie. root -> a -> (n s):
;;
;; ("" 2
;;   (a 2
;;     (s 1 .)
;;     (n 1 .)))
;;
;; Or when compact is true:
;; ("" 2 (a 2 (s 1 (T 1)) (n 1 (T 1))))

(defun print-words (trie &optional (stream t) (prefix ""))
  ; Could use more arguments: start end &key (with-count))
  "Prints words from the trie, one per line. Returns total word count.

  Options:
  * with-count: Prints word counts after tab when over one.

  TODO:
  * Use keyword arguments?
  * Implement start, end
  * Allow to specify separator instead of newline"

  (when (leafp trie)
    (let ((width (width trie)))
      ;; Print the word and it's count, if the count is not 0 or 1.
      ;; Logand makes 1 or 0 => 0 to suppress printing.
      (format t "~A~:[~;~10t~d~]~%" prefix (/= 0 (logand -2 width)) width))
    (return-from print-words))

  (loop as branch in (branches trie) do
    (print-words branch stream
      (concatenate 'string prefix (format nil "~A" (key trie)))))

  (when (equal "" (key trie))
    (width trie)))

(defun print-trie-simple (trie &optional (stream t) (depth 0) (indent 2))
  "## Traverse tries printing out nodes"
  (when indent (format stream "~&~v@T" (* depth indent)))
  (format stream "(~A ~d" (key trie) (width trie))
  (unless (leafp trie)
    (loop as branch in (branches trie) do
      (print-trie-simple branch stream (+ 1 depth) indent)))
  (format stream ")"))

(defmethod print-object ((trie trie) stream)
  (print-trie-to-stream trie stream))

(defmethod print-trie-to-stream ((trie trie) stream &optional (depth 0) (compact t))
  "Pretty print the trie."
  (let ((*standard-output* stream)
        (key (key trie)))

    ; Indent and write opening parens
    (format stream "~v@T" (* 2 depth))
    (write-char #\( ) ; <- Left parenthesis character!

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
           (format stream "~%") ; Newline if necessary
           (print-trie-to-stream branch stream (+ 1 depth) compact))
         (write-char #\) )))) ; <- Right Parenthesis character!

(defmethod traverse ((trie trie) (fun function) &key (do-leafs nil))
  "Traverse the trie perfoming a function on each node."
  (when (leafp trie)
    (return-from traverse (when do-leafs (funcall fun trie))))
  (loop as node in (branches trie) do
       (progn
         (funcall fun trie)
         (traverse node fun))))
