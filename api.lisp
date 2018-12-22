;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :split-sequence)

(defmacro check-bounds (sequence start end)
  (let ((length (gensym (string '#:length))))
    `(let ((,length (length ,sequence)))
       (check-type ,start unsigned-byte "a non-negative integer")
       (when ,end (check-type ,end unsigned-byte "a non-negative integer or NIL"))
       (unless ,end
         (setf ,end ,length))
       (unless (<= ,start ,end ,length)
         (error "Wrong sequence bounds. start: ~S end: ~S" ,start ,end)))))

(defun check-tests (test test-not)
  (when (and test test-not)
    (error "Cannot specify both TEST and TEST-NOT.")))

(declaim (ftype (function (&rest t) (values list integer))
                split-sequence split-sequence-if split-sequence-if-not))

(defun split-sequence (delimiter sequence &key (start 0) (end nil) (from-end nil)
                                            (count nil) (remove-empty-subseqs nil)
                                            (test #'eql test-p) (test-not nil test-not-p)
                                            (key #'identity))
  (check-bounds sequence start end)
  (check-tests test-p test-not-p)
  (etypecase sequence
    (list (split-list delimiter sequence start end from-end count
                      remove-empty-subseqs test test-not key))
    (vector (split-vector delimiter sequence start end from-end count
                          remove-empty-subseqs test test-not key))
    #+(or sbcl abcl)
    (extended-sequence (split-extended-sequence delimiter sequence start end from-end count
                                                remove-empty-subseqs test test-not key))))

(defun split-sequence-if (predicate sequence &key (start 0) (end nil) (from-end nil)
                                               (count nil) (remove-empty-subseqs nil) (key #'identity))
  (check-bounds sequence start end)
  (etypecase sequence
    (list (split-list-if predicate sequence start end from-end count
                         remove-empty-subseqs key))
    (vector (split-vector-if predicate sequence start end from-end count
                             remove-empty-subseqs key))
    #+(or sbcl abcl)
    (extended-sequence (split-extended-sequence-if predicate sequence start end from-end count
                                                   remove-empty-subseqs key))))

(defun split-sequence-if-not (predicate sequence &key (start 0) (end nil) (from-end nil)
                                                   (count nil) (remove-empty-subseqs nil) (key #'identity))
  (check-bounds sequence start end)
  (etypecase sequence
    (list (split-list-if-not predicate sequence start end from-end count
                             remove-empty-subseqs key))
    (vector (split-vector-if-not predicate sequence start end from-end count
                                 remove-empty-subseqs key))
    #+(or sbcl abcl)
    (extended-sequence (split-extended-sequence-if-not predicate sequence start end from-end count
                                                       remove-empty-subseqs key))))

(pushnew :split-sequence *features*)
