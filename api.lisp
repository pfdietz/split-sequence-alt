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

(setf (documentation 'split-sequence 'function)
      "Return a list of subsequences in seq delimited by delimiter.
If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped.")

(setf (documentation 'split-sequence-if 'function)
      "Return a list of subsequences in seq delimited by items satisfying
predicate.
If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped.")

(setf (documentation 'split-sequence-if-not 'function)
      "Return a list of subsequences in seq delimited by items satisfying
\(CL:COMPLEMENT predicate).
If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF-NOT.  In particular,
the behaviour of :from-end is possibly different from other versions
of this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped.")

(pushnew :split-sequence *features*)
