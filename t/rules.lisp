(in-package :cl-bnf-tests)

(5am:def-suite test-rule-suite
  :description "Test for rules.")

(5am:in-suite test-rule-suite)

(define-rule or-rule-with-function
    (:or #'alpha-char-p #'numeric-char-p))

(5am:def-test test-or-rule-with-function ()
  (5am:is (and
	   (char-equal (parse #'or-rule-with-function "1") #\1)
	   (char-equal (parse #'or-rule-with-function "a") #\a)
	   (equal (parse #'or-rule-with-function "-") nil))))

(define-rule and-rule-with-functions
    (:and #'alpha-char-p #'numeric-char-p))

(5am:def-test test-and-rule-with-functions ()
  (5am:is (and
	   (equal (parse #'and-rule-with-functions "a1") '(#\a #\1))
	   (equal (parse #'and-rule-with-functions "1b") nil))))
