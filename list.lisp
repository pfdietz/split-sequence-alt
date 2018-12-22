;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :split-sequence)

(declaim (#-split-sequence/cover inline
          #+split-sequence/cover notinline
          split-list split-list-if split-list-if-not))

(declaim (ftype (function (&rest t) (values list integer))
                split-list split-list-if split-list-if-not))

(declaim (ftype (function (function list integer (or null integer) (or null integer) boolean function)
                          (values list integer))
                split-list-from-start split-list-from-end split-list-internal))

#+split-sequence/cover(cover:annotate t)

(defun split-list
    (delimiter list start end from-end count remove-empty-subseqs test test-not key)
  (let ((predicate (if test-not
                       (lambda (x) (not (funcall test-not delimiter x)))
                       (lambda (x) (funcall test delimiter x)))))
    (if from-end
        (split-list-from-end predicate list start end count remove-empty-subseqs key)
        (split-list-from-start predicate list start end count remove-empty-subseqs key))))

(defun split-list-if (predicate list start end from-end count remove-empty-subseqs key)
  (if from-end
      (split-list-from-end predicate list start end count remove-empty-subseqs key)
      (split-list-from-start predicate list start end count remove-empty-subseqs key)))

(defun split-list-if-not (predicate list start end from-end count remove-empty-subseqs key)
  (split-list-if (complement predicate) list start end from-end count remove-empty-subseqs key))

(defun split-list-from-end (predicate list start end count remove-empty-subseqs key)
  (let ((length (length list)))
    (multiple-value-bind (result index)
        (split-list-internal predicate (nreverse (copy-list list))
                             (- length end) (- length start) count remove-empty-subseqs key)
      (loop for cons on result
            for car = (car cons)
            do (setf (car cons) (nreverse car)))
      (values (nreverse result) (- length index)))))

(defun split-list-from-start (predicate list start end count remove-empty-subseqs key)
  (split-list-internal predicate (copy-list list) start end count remove-empty-subseqs key))

(defun split-list-internal (predicate list start end count remove-empty-subseqs key)
  (let ((end (or end (length list)))
        (current-first nil) (current-last nil) (next-cons (nthcdr start list))
        (result '()) (nr-elts 0))
    (do ((n start (1+ n)))
        (nil)
      (cond ((or (endp next-cons)
                 (and count (>= nr-elts count))
                 (<= end n))
             (unless (or (and count (>= nr-elts count))
                         (and remove-empty-subseqs (null current-first)))
               (push current-first result))
             (when (and (= end n) current-first)
               (setf (cdr current-last) nil))
             (when (and remove-empty-subseqs (< n end))
               (loop :while next-cons
                     :while (< n end)
                     :while (funcall predicate (funcall key (car next-cons)))
                     :do (setf next-cons (cdr next-cons))
                         (incf n)))
             (return (values (nreverse result) n)))
            ((funcall predicate (funcall key (car next-cons)))
             (unless (and remove-empty-subseqs (null current-first))
               (push current-first result)
               (incf nr-elts))
             (when current-last
               (setf (cdr current-last) nil))
             (setf next-cons (cdr next-cons)
                   current-first nil
                   current-last nil))
            (t
             (setf current-last next-cons)
             (unless current-first
               (setf current-first next-cons))
             (setf next-cons (cdr next-cons)))))))

#+split-sequence/cover (cover:annotate nil)
