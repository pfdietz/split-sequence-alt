;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :split-sequence)

(declaim (inline list-split-sequence list-split-sequence-if list-split-sequence-if-not))

(declaim (ftype (function (&rest t) (values list integer))
                list-split-sequence list-split-sequence-if list-split-sequence-if-not))

(defun list-split-sequence (delimiter sequence start end from-end count
                            remove-empty-subseqs test test-not key)
  (declare (ignore delimiter sequence start end from-end count
                   remove-empty-subseqs test test-not key))
  (values '() 0)) ;; TODO

(defun list-split-sequence-if (predicate sequence start end from-end count
                               remove-empty-subseqs key)
  (declare (ignore predicate sequence start end from-end count
                   remove-empty-subseqs key))
  (values '() 0)) ;; TODO

(defun list-split-sequence-if-not (predicate sequence start end from-end count
                                   remove-empty-subseqs key)
  (declare (ignore predicate sequence start end from-end count
                   remove-empty-subseqs key))
  (values '() 0)) ;; TODO
