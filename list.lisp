;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :split-sequence)

(declaim (inline split-list split-list-if split-list-if-not))

(declaim (ftype (function (&rest t) (values list integer))
                split-list split-list-if split-list-if-not))

(declaim (ftype (function (function list integer (or null integer) (or null integer) boolean)
                          (values list integer))
                split-list-from-start split-list-from-end))

(defun split-list
    (delimiter list start end from-end count remove-empty-subseqs test test-not key)
  (cond
    ((and (not from-end) (null test-not))
     (split-list-from-start (lambda (list start)
                              (position delimiter list :start start :key key :test test))
                            list start end count remove-empty-subseqs))
    ((and (not from-end) test-not)
     (split-list-from-start (lambda (list start)
                              (position delimiter list :start start :key key :test-not test-not))
                            list start end count remove-empty-subseqs))
    ((and from-end (null test-not))
     (split-list-from-end (lambda (list end) ;; TODO copy and reverse the list here
                            (position delimiter list :end end :from-end t :key key :test test))
                          list start end count remove-empty-subseqs))
    (t
     (split-list-from-end (lambda (list end) ;; TODO copy and reverse the list here
                            (position delimiter list :end end :from-end t :key key :test-not test-not))
                          list start end count remove-empty-subseqs))))

(defun split-list-if
    (predicate list start end from-end count remove-empty-subseqs key)
  (if from-end
      (split-list-from-end (lambda (list end) ;; TODO copy and reverse the list here
                             (position-if predicate list :end end :from-end t :key key))
                           list start end count remove-empty-subseqs)
      (split-list-from-start (lambda (list start)
                               (position-if predicate list :start start :key key))
                             list start end count remove-empty-subseqs)))

(defun split-list-if-not
    (predicate list start end from-end count remove-empty-subseqs key)
  (if from-end
      (split-list-from-end (lambda (list end) ;; TODO copy and reverse the list here
                             (position-if-not predicate list :end end :from-end t :key key))
                           list start end count remove-empty-subseqs)
      (split-list-from-start (lambda (list start)
                               (position-if-not predicate list :start start :key key))
                             list start end count remove-empty-subseqs)))

(defun split-list-from-start (position-fn list start end count remove-empty-subseqs)
  (declare (ignore position-fn list start end count remove-empty-subseqs))
  (values '() 0))

(defun split-list-from-end (position-fn list start end count remove-empty-subseqs)
  (declare (ignore position-fn list start end count remove-empty-subseqs))
  (values '() 0))
