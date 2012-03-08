;;; -*- coding: utf-8; mode: Lisp; syntax: ANSI-Common-Lisp -*-
;;;
;;; -------------------------------------------------------------------------
;;; DAG - a generic tree graph implementation with vertex widths
;;; -------------------------------------------------------------------------
;;;
;;; Copyright (c) 2008-2011 Peter Hillerström, All Rights Reserved
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

(defpackage #:nu.composed.dag
  (:documentation "Directed acyclic graph implementation with vertex widths. Trees are an example of directed acyclic graphs. Can be used for tries also.")
  (:nicknames :dag)
  (:use :cl :arnesi :lift)
  (:export #:node
           #:vertex
           #:edge
           #:arc
           #:key
           #:width))

(in-package #:nu.composed.dag)

(setq *print-pretty* t)
(setq *print-circle* nil)
(setq *print-level* 12)

;;; Initialization

(deftype arc ()
  (cons (or 'node nil) (or 'node nil)))

(defclass node ()
  ((edges
    :type list
    :initform nil
    :accessor edges
    :initarg :edges))
  (:documentation "A node (vertex) connects to other nodex through many edges"))

(defmethod print-object ((node node) s)
  (let ((*standard-output* s)
        (edges (edges node)))
    (write-char #\()
    (write (car edges) :stream s)
    (format s "~a" "->")
    (write (cadr edges) :stream s)
    (write-char #\))))

(defclass edge ()
  ((label
    :type atom ; atom is equal to (not cons)
    :initform ""
    :reader label
    :initarg :label
    :documentation "Key is the label for an edge. Can be any type for generality.")
   (width
    :type (integer 0 *)
    :initform 0
    :accessor width
    :initarg :width)
   (join
    :type arc
    :initform (cons nil nil)
    :accessor join
    :initarg join))
   (:documentation "A line (edge) connects two nodes (vertices) together."))

(defmethod print-object ((edge edge) stream)
  (let ((*standard-output* stream))
    (format stream "~s" (label edge))))
