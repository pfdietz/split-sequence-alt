;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :split-sequence)

(declaim (inline split-list split-list-if split-list-if-not))

(declaim (ftype (function (&rest t) (values list integer))
                split-list split-list-if split-list-if-not))

(declaim (ftype (function (function list integer (or null integer) (or null integer) boolean function)
                          (values list integer))
                split-list-from-start split-list-from-end split-list-internal))

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
             ;; (format t "Finishing loop~%")
             (unless (and count (>= nr-elts count))
               (unless (and remove-empty-subseqs (null current-first))
                 ;; (format t "Pushing ~S to result~%" current-first)
                 (push current-first result)))
             (when (and (= end n) current-first)
               (setf (cdr current-last) nil))
             (when (and remove-empty-subseqs (< n end))
               (loop :while (and next-cons (< n end)
                                 (funcall predicate (funcall key (car next-cons))))
                     :do ;; (format t "Bumping up a cons~%")
                         (setf next-cons (cdr next-cons))
                         (incf n)))
             (return (values (nreverse result) n)))
            ((funcall predicate (funcall key (car next-cons)))
             ;;(format t "Splitting on ~S~%" (funcall key (car next-cons)))
             (unless (and remove-empty-subseqs (null current-first))
               ;; (format t "Pushing ~S to result~%" current-first)
               (push current-first result)
               (incf nr-elts))
             (setf next-cons (cdr next-cons))
             (when current-last (setf (cdr current-last) nil))
             (setf current-first nil
                   current-last nil))
            (t
             ;; (format t "Progressing with ~S~%" next-cons)
             (setf current-last next-cons)
             (unless current-first
               (setf current-first next-cons)
               ;; (format t "Setting CURRENT-FIRST to ~S~%" next-cons)
               )
             (setf next-cons (cdr next-cons)))))))
