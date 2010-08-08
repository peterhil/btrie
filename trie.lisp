;; Weighted branches trie
;;; 
;;; A trie implementation with nodes holding a width value;
;;; an integer telling how many branches go through each node.
;;; 
;;; Author: Peter Hillerström
;;; Initial version: 2010-08-01
;;; 
;;; This trie implementation can hold nodes with keys of any type, 
;;; and can manipulate tries with any type of sequence as keys.


;; Example of representation:
;; '(ROOT 2 ((F 2 ((G 1 NIL) (H 1 NIL))))) ; ROOT -> F -> (G E)

(defpackage :trie)
; (:use :cl)

(defparameter *print-circle* t)
(defparameter *print-pretty* t)
(defparameter *debug* nil)

; Struct node occupies 24 bits for slots and 16 for 1-char key = 40 bits
(defstruct
    (trie
        (:type list)
        ; :named
        )
    (key "" :read-only t)   ; generic - could be :type char
    (width 0 :type integer)
    (branches nil :type list))  ; Could a vector for optimization, but complicates things!

(defun test ()
    (setf r (make-trie))
    (add-seqs r '("blow" "boa" "blush" "foo")))

; Sentence level

(defun add-seqs (trie seqs)
    (mapcar #'(lambda (seq) (add-seq trie seq))
        seqs))

; Word level

(defun find-seq (trie seq)
    "Find a sequence from trie"
    (when (zerop (length seq))
        (return-from find-seq trie))
    (let ((symbol (find-key trie (elt seq 0))))
        (if (null symbol)
            (return-from find-seq nil)
            (find-seq symbol (subseq seq 1)))))

; (defun remove-seq (trie seq &optional (count 1))
;     "Remove a sequence from trie"
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
;             (remove-key trie (elt seq 0))
;             (return-from remove-seq nil))
;         (remove-seq symbol (subseq seq 1) count)))

(defun add-seq (trie seq &optional (count 1))
    "Add a branch to the trie count times. If branch already exists, increase it’s width.
    A count below one is changed to one. Modifies trie in-place."
    (when (< count 0) (error (format nil "Negative count ~A." count)))
    (incf (trie-width trie) count)
    (if (zerop (length seq))
        (progn (add-key trie t count)
               (return-from add-seq trie)))
    (let ((symbol (add-key trie (elt seq 0) 0)))
        ; (add-key trie (elt seq 0) count)
        (add-seq symbol (subseq seq 1) count)))

; Node or symbol level

(defun wordp (trie)
    (find-key trie t))

(defun leafp (trie)
    (car (trie-branches trie)))

(defun find-key (trie key)
    "Return a symbol matching key from trie's branches."
    (find key (trie-branches trie) :test #'equal :key (lambda (n) (trie-key n))))

(defun add-key (trie key &optional (count 1))
    "Add a node to trie. If node exists, increases it’s width."
    (when (< count 0) (error (format nil "Negative count ~A." count)))
    (let ((node (find-key trie key)))
        (if node
            (incf (trie-width node) count)
            (setq node (create-node trie key count)))
        node))

(defun create-node (trie key &optional (count 0))
    "Destructively adds node to trie"
    (car (push (make-trie :key key :width count) (trie-branches trie))))

(defun remove-key (trie key &optional (count 1))
    "Add a node to trie. If node exists, increases it’s width.")

(defun remove-key-broken (trie key)
    (let ((node (find-key trie key))
          (branches (trie-branches trie)))
        (when node
            (decf (trie-width node)) ; FIXME
            (when (<= 0 trie-width node)
                (setf branches (remove-if #'(lambda (x) (equal x key)) branches))))))


;; Traversal & printing

; (defmethod print-object ((object trie) stream)
;     (when *print-pretty* (format stream "~&~v@T" 2))
;     (format stream "(~S ~d" (trie-key trie) (trie-width trie))
;     (when (leafp trie)
;         (loop as branch in (trie-branches trie) do
;             (print branch)))
;     (format stream ")"))

(defun pprint-trie (*standard-output* trie &key (raw nil))
    (setf *print-miser-width* nil)  ; Miser mode disables pprint-indent!
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
        (if (leafp trie)
            ; Has branches
            (progn
                ; Print list of Branches
                (write-char #\Space)
                (when *debug* (write :+))
                (loop as branch in (trie-branches trie) do
                    (when *debug* (write :~))
                    ; Word endings
                    (when (and (not raw) (wordp trie))
                        (write-char #\•)
                        (return))
                    (pprint-newline :mandatory) ; THIS makes the newline after leaf!
                    (pprint-indent :current 0)
                    (pprint-trie *standard-output* branch :raw raw) ; Next level
                    (pprint-exit-if-list-exhausted)                 ; Exit after leaf
                    (when *debug* (write :*))
                    )
                (when *debug* (write :@))
                )
            ; Is leaf
            (progn
                (when *debug* (write :%))
                (pprint-exit-if-list-exhausted)))))

(defun print-trie (trie &optional (depth 0) &key (indent nil))
    "Traverse tries printing out nodes"
    (when indent (format t "~&~v@T" (* depth 2)))
    (format t "(~S ~d" (trie-key trie) (trie-width trie))
    (when (leafp trie)
        (loop as branch in (trie-branches trie) do
            (print-trie branch (+ 1 depth) :indent indent)))
    (format t ")"))

(defun print-words (trie &optional order)
    "Prints words from the trie"
    nil)


; Macro versions of trie recursing functions - beware of bugs!
; 
; (defmacro wrap-recursion ((fn &rest args) &body body)
;     `(funcall ,fn ,@args ,@body))
; 
; (defun recurse-trie (trie seq &rest body)
;     "A macro to do operations on trie nodes following a sequence recursively.
;     Provides key, symbol and suffix in addition to arguments. Returns if sequence is 
;     empty, returning the current trie node. Otherwise needs a return-from form."
;     (if (zerop (length seq))
;         (return-from recurse-trie trie))
;     (let* ((key (elt seq 0))
;            (symbol (find-key trie key))
;            (suffix (subseq seq 1)))
;         (macroexpand `(,@body)) ; Might not work and even go into inf.loop when compiling!
;         (recurse-trie symbol suffix)))
; 
; (defun find-seq! (trie seq)
;     (wrap-recursion (#'recurse-trie trie seq)
;         (unless symbol
;             (return-from find-seq! nil))))
; 
; (defun add-seq! (trie seq)
;     (wrap-recursion (#'recurse-trie trie seq)
;         (unless symbol
;             (setf symbol (add-key trie (elt seq 0))))
;         (incf (trie-width symbol))))
