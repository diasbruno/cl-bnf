(in-package :cl-bnf-tests)

(5am:def-suite test-rule-suite
  :description "Test for rules.")

(5am:in-suite test-rule-suite)

(define-rule or-rule-with-function
    (:or #'alpha-char-p #'numeric-char-p))

(5am:def-test test-or-rule-with-function ()
  (let ((example1 (make-string-input-stream "1"))
	(example2 (make-string-input-stream "a"))
	(example3 (make-string-input-stream "-")))
   (5am:is (and
	    (char-equal (parse #'or-rule-with-function example1) #\1)
	    (char-equal (parse #'or-rule-with-function example2) #\a)
	    (equal (parse #'or-rule-with-function example3) nil)))))

(define-rule and-rule-with-functions
    (:and #'alpha-char-p #'numeric-char-p))

(5am:def-test test-and-rule-with-functions ()
  (let ((example1 (make-string-input-stream "a1"))
	(example2 (make-string-input-stream "1b")))
   (5am:is (and
	    (equal (parse #'and-rule-with-functions example1) '(#\a #\1))
	    (equal (parse #'and-rule-with-functions example2) nil)))))
