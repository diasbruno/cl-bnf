(in-package :cl-bnf-tests)

(5am:def-suite test-matchers-suite
  :description "Test for matchers.")

(5am:in-suite test-matchers-suite)

(5am:def-test match-pred-matcher ()
  (flet ((is-char-a (x) (char-equal #\a x)))
    (let* ((stream (make-string-input-stream "a"))
           (expected (list :match stream #\a)))
      (5am:is (equal
               (cl-bnf:match-pred stream #'is-char-a)
               expected)))))

(5am:def-test single-char-matcher ()
  (let* ((stream (make-string-input-stream "a"))
        (expected (list :match stream #\a)))
    (5am:is (equal
             (cl-bnf:single-char stream #\a)
             expected))))

(5am:def-test string-matcher ()
  (let* ((stream (make-string-input-stream "abc"))
        (expected (list :match stream "abc")))
    (5am:is (equal
             (cl-bnf:string-match stream "abc")
             expected))))

(5am:def-test maybe-matcher ()
  (let* ((stream (make-string-input-stream "1"))
         (expected (list :match stream #\1)))
    (5am:is (equal
             (cl-bnf:maybe-match stream (list nil #'numeric-char-p))
             expected))))

(5am:run 'many-matcher)

(5am:def-test many-matcher ()
  (let* ((stream (make-string-input-stream "11"))
         (expected (list :match stream '(#\1 #\1))))
    (5am:is (equal
             (cl-bnf:many-matches stream '#'numeric-char-p)
             expected))))
