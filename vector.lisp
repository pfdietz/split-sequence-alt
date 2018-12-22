;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :split-sequence)

(declaim (inline vector-split-sequence vector-split-sequence-if vector-split-sequence-if-not))

(declaim (ftype (function (&rest t) (values list integer))
                vector-split-sequence vector-split-sequence-if vector-split-sequence-if-not))

(defun vector-split-sequence
    (delimiter sequence start end from-end count remove-empty-subseqs test test-not key)
  (cond
    ((and (not from-end) (null test-not))
     (vector-split-from-start (lambda (sequence start)
                                (position delimiter sequence :start start :key key :test test))
                              sequence start end count remove-empty-subseqs))
    ((and (not from-end) test-not)
     (vector-split-from-start (lambda (sequence start)
                                (position delimiter sequence :start start :key key :test-not test-not))
                              sequence start end count remove-empty-subseqs))
    ((and from-end (null test-not))
     (vector-split-from-end (lambda (sequence end)
                              (position delimiter sequence :end end :from-end t :key key :test test))
                            sequence start end count remove-empty-subseqs))
    (t
     (vector-split-from-end (lambda (sequence end)
                              (position delimiter sequence :end end :from-end t :key key :test-not test-not))
                            sequence start end count remove-empty-subseqs))))

(defun vector-split-sequence-if
    (predicate sequence start end from-end count remove-empty-subseqs key)
  (if from-end
      (vector-split-from-end (lambda (sequence end)
                               (position-if predicate sequence :end end :from-end t :key key))
                             sequence start end count remove-empty-subseqs)
      (vector-split-from-start (lambda (sequence start)
                                 (position-if predicate sequence :start start :key key))
                               sequence start end count remove-empty-subseqs)))

(defun vector-split-sequence-if-not
    (predicate sequence start end from-end count remove-empty-subseqs key)
  (if from-end
      (vector-split-from-end (lambda (sequence end)
                               (position-if-not predicate sequence :end end :from-end t :key key))
                             sequence start end count remove-empty-subseqs)
      (vector-split-from-start (lambda (sequence start)
                                 (position-if-not predicate sequence :start start :key key))
                               sequence start end count remove-empty-subseqs)))

(deftype array-index (&optional (length array-dimension-limit))
  `(integer 0 (,length)))

(declaim (ftype (function (function vector array-index
                                    (or null array-index) (or null array-index) boolean)
                          (values list integer))
                vector-split-from-start vector-split-from-end))

(defun vector-split-from-end (position-fn sequence start end count remove-empty-subseqs)
  (declare (optimize (speed 3) (debug 0))
           (type (function (sequence fixnum) (or null fixnum)) position-fn))
  (loop
    :for right := end :then left
    :for left := (max (or (funcall position-fn sequence right) -1)
                      (1- start))
    :unless (and (= right (1+ left))
                 remove-empty-subseqs) ; empty subseq we don't want
      :if (and count (>= nr-elts count))
        ;; We can't take any more. Return now.
        :return (values (nreverse subseqs) right)
    :else
      :collect (subseq sequence (1+ left) right) into subseqs
      :and :sum 1 :into nr-elts :of-type fixnum
    :until (< left start)
    :finally (return (values (nreverse subseqs) (1+ left)))))

(defun vector-split-from-start (position-fn sequence start end count remove-empty-subseqs)
  (declare (optimize (speed 3) (debug 0))
           (type vector sequence)
           (type (function (sequence fixnum) (or null fixnum)) position-fn))
  (let ((length (length sequence)))
    (loop
      :for left := start :then (1+ right)
      :for right := (min (or (funcall position-fn sequence left) length)
                         end)
      :unless (and (= right left)
                   remove-empty-subseqs) ; empty subseq we don't want
        :if (and count (>= nr-elts count))
          ;; We can't take any more. Return now.
          :return (values subseqs left)
      :else
        :collect (subseq sequence left right) :into subseqs
        :and :sum 1 :into nr-elts :of-type fixnum
      :until (>= right end)
      :finally (return (values subseqs right)))))
