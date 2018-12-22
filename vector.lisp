;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :split-sequence)

(declaim (#-split-sequence/cover inline
          #+split-sequence/cover notinline
          split-vector split-vector-if split-vector-if-not))

(deftype array-index (&optional (length array-dimension-limit))
  `(integer 0 (,length)))

(declaim (ftype (function (&rest t) (values list integer))
                split-vector split-vector-if split-vector-if-not))

(declaim (ftype (function (function vector array-index
                                    (or null array-index) (or null array-index) boolean)
                          (values list integer))
                split-vector-from-start split-vector-from-end))

#+split-sequence/cover (cover:annotate t)

(defun split-vector
    (delimiter vector start end from-end count remove-empty-subseqs test test-not key)
  (cond
    ((and (not from-end) (null test-not))
     (split-vector-from-start (lambda (vector start)
                                (position delimiter vector :start start :key key :test test))
                              vector start end count remove-empty-subseqs))
    ((and (not from-end) test-not)
     (split-vector-from-start (lambda (vector start)
                                (position delimiter vector :start start :key key :test-not test-not))
                              vector start end count remove-empty-subseqs))
    ((and from-end (null test-not))
     (split-vector-from-end (lambda (vector end)
                              (position delimiter vector :end end :from-end t :key key :test test))
                            vector start end count remove-empty-subseqs))
    (t
     (split-vector-from-end (lambda (vector end)
                              (position delimiter vector :end end :from-end t :key key :test-not test-not))
                            vector start end count remove-empty-subseqs))))

(defun split-vector-if
    (predicate vector start end from-end count remove-empty-subseqs key)
  (if from-end
      (split-vector-from-end (lambda (vector end)
                               (position-if predicate vector :end end :from-end t :key key))
                             vector start end count remove-empty-subseqs)
      (split-vector-from-start (lambda (vector start)
                                 (position-if predicate vector :start start :key key))
                               vector start end count remove-empty-subseqs)))

(defun split-vector-if-not
    (predicate vector start end from-end count remove-empty-subseqs key)
  (if from-end
      (split-vector-from-end (lambda (vector end)
                               (position-if-not predicate vector :end end :from-end t :key key))
                             vector start end count remove-empty-subseqs)
      (split-vector-from-start (lambda (vector start)
                                 (position-if-not predicate vector :start start :key key))
                               vector start end count remove-empty-subseqs)))

(defun split-vector-from-end (position-fn vector start end count remove-empty-subseqs)
  (declare (optimize (speed 3) (debug 0))
           (type (function (vector fixnum) (or null fixnum)) position-fn))
  (loop
    :for right := end :then left
    :for left := (max (or (funcall position-fn vector right) -1)
                      (1- start))
    :unless (and (= right (1+ left))
                 remove-empty-subseqs) ; empty subseq we don't want
      :if (and count (>= nr-elts count))
        ;; We can't take any more. Return now.
        :return (values (nreverse subseqs) right)
    :else
      :collect (subseq vector (1+ left) right) into subseqs
      :and :sum 1 :into nr-elts :of-type fixnum
    :until (< left start)
    :finally (return (values (nreverse subseqs) (1+ left)))))

(defun split-vector-from-start (position-fn vector start end count remove-empty-subseqs)
  (declare (optimize (speed 3) (debug 0))
           (type vector vector)
           (type (function (vector fixnum) (or null fixnum)) position-fn))
  (let ((length (length vector)))
    (loop
      :for left := start :then (1+ right)
      :for right := (min (or (funcall position-fn vector left) length)
                         end)
      :unless (and (= right left)
                   remove-empty-subseqs) ; empty subseq we don't want
        :if (and count (>= nr-elts count))
          ;; We can't take any more. Return now.
          :return (values subseqs left)
      :else
        :collect (subseq vector left right) :into subseqs
        :and :sum 1 :into nr-elts :of-type fixnum
      :until (>= right end)
      :finally (return (values subseqs right)))))

#+split-sequence/cover (cover:annotate nil)
