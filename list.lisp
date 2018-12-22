;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :split-sequence)

(declaim (inline list-split-sequence list-split-sequence-if list-split-sequence-if-not))

(declaim (ftype (function (&rest t) (values list integer))
                list-split-sequence list-split-sequence-if list-split-sequence-if-not))

(defun list-split-sequence
    (delimiter sequence start end from-end count remove-empty-subseqs test test-not key)
  (cond
    ((and (not from-end) (null test-not))
     (list-split-from-start (lambda (sequence start)
                              (position delimiter sequence :start start :key key :test test))
                            sequence start end count remove-empty-subseqs))
    ((and (not from-end) test-not)
     (list-split-from-start (lambda (sequence start)
                              (position delimiter sequence :start start :key key :test-not test-not))
                            sequence start end count remove-empty-subseqs))
    ((and from-end (null test-not))
     (list-split-from-end (lambda (sequence end) ;; TODO copy and reverse the list here
                            (position delimiter sequence :end end :from-end t :key key :test test))
                          sequence start end count remove-empty-subseqs))
    (t
     (list-split-from-end (lambda (sequence end) ;; TODO copy and reverse the list here
                            (position delimiter sequence :end end :from-end t :key key :test-not test-not))
                          sequence start end count remove-empty-subseqs))))

(defun list-split-sequence-if
    (predicate sequence start end from-end count remove-empty-subseqs key)
  (if from-end
      (list-split-from-end (lambda (sequence end) ;; TODO copy and reverse the list here
                             (position-if predicate sequence :end end :from-end t :key key))
                           sequence start end count remove-empty-subseqs)
      (list-split-from-start (lambda (sequence start)
                               (position-if predicate sequence :start start :key key))
                             sequence start end count remove-empty-subseqs)))

(defun list-split-sequence-if-not
    (predicate sequence start end from-end count remove-empty-subseqs key)
  (if from-end
      (list-split-from-end (lambda (sequence end) ;; TODO copy and reverse the list here
                             (position-if-not predicate sequence :end end :from-end t :key key))
                           sequence start end count remove-empty-subseqs)
      (list-split-from-start (lambda (sequence start)
                               (position-if-not predicate sequence :start start :key key))
                             sequence start end count remove-empty-subseqs)))

(declaim (ftype (function (function list integer (or null integer) (or null integer) boolean)
                          (values list integer))
                list-split-from-start list-split-from-end))

(defun list-split-from-start (position-fn sequence start end count remove-empty-subseqs)
  (declare (ignore position-fn sequence start end count remove-empty-subseqs))
  (values '() 0))

(defun list-split-from-end (position-fn sequence start end count remove-empty-subseqs)
  (declare (ignore position-fn sequence start end count remove-empty-subseqs))
  (values '() 0))
