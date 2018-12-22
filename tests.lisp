;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage :split-sequence/tests
  (:use :common-lisp :split-sequence :fiveam))

(in-package :split-sequence/tests)

(in-suite* :split-sequence)

(defmacro define-test (name (&key input output index) &body forms)
  ;; This macro automatically generates test code for testing vector and list input.
  ;; Vector input and output is automatically coerced into list form for the list tests.
  ;; (DEFINE-TEST FOO ...) generates FIVEAM tests FOO.VECTOR and FOO.LIST.
  (check-type name symbol)
  (check-type input (cons symbol (cons vector null)))
  (check-type output (cons symbol (cons list null)))
  (check-type index (cons symbol (cons unsigned-byte null)))
  (let* ((input-symbol (first input)) (vector-input (second input))
         (output-symbol (first output)) (vector-output (second output))
         (index-symbol (first index)) (index-value (second index))
         (list-input (coerce vector-input 'list))
         (list-output (mapcar (lambda (x) (coerce x 'list)) vector-output))
         (vector-name (intern (concatenate 'string (symbol-name name) ".VECTOR")))
         (list-name (intern (concatenate 'string (symbol-name name) ".LIST"))))
    `(progn
       (test (,vector-name :compile-at :definition-time)
         (let ((,input-symbol ',vector-input)
               (,output-symbol ',vector-output)
               (,index-symbol ,index-value))
           ,@forms))
       (test (,list-name :compile-at :definition-time)
         (let ((,input-symbol ',list-input)
               (,output-symbol ',list-output)
               (,index-symbol ,index-value))
           ,@forms)))))

(define-test split-sequence.1 (:input (input "a;;b;c")
                               :output (output ("a" "" "b" "c"))
                               :index (index 6))
  (is (equalp (split-sequence #\; input)
              (values output index))))

(define-test split-sequence.2 (:input (input "a;;b;c")
                               :output (output ("a" "" "b" "c"))
                               :index (index 0))
  (is (equalp (split-sequence #\; input :from-end t)
              (values output index))))

(define-test split-sequence.3 (:input (input "a;;b;c")
                               :output (output ("c"))
                               :index (index 4))
  (is (equalp (split-sequence #\; input :from-end t :count 1)
              (values output index))))

(define-test split-sequence.4 (:input (input "a;;b;c")
                               :output (output ("a" "b" "c"))
                               :index (index 6))
  (is (equalp (split-sequence #\; input :remove-empty-subseqs t)
              (values output index))))

(define-test split-sequence.5 (:input (input ";oo;bar;ba;")
                               :output (output ("oo" "bar" "b"))
                               :index (index 9))
  (is (equalp (split-sequence #\; input :start 1 :end 9)
              (values output index))))

(define-test split-sequence.6 (:input (input "abracadabra")
                               :output (output ("" "br" "c" "d" "br" ""))
                               :index (index 11))
  (is (equalp (split-sequence #\A input :key #'char-upcase)
              (values output index))))

(define-test split-sequence-if.1 (:input (input "abracadabra")
                                  :output (output ("" "" "r" "c" "d" "" "r" ""))
                                  :index (index 11))
  (is (equalp (split-sequence-if (lambda (x) (member x '(#\a #\b))) input)
              (values output index))))

(define-test split-sequence-if-not.1 (:input (input "abracadabra")
                                      :output (output ("ab" "a" "a" "ab" "a"))
                                      :index (index 11))
  (is (equalp (split-sequence-if-not (lambda (x) (member x '(#\a #\b))) input)
              (values output index))))
