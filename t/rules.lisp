(in-package :cl-bnf-tests)

(5am:def-suite test-rule-suite
  :description "Test for rules.")

(5am:in-suite test-rule-suite)

(cl-bnf:define-rule or-rule-with-function
    (:or #'alpha-char-p #'numeric-char-p))

(5am:def-test test-or-rule-with-function ()
  (with-input-stream (s "1")
    (5am:is (char-equal (cl-bnf:parse #'or-rule-with-function s) #\1)))
  (with-input-stream (s "a")
    (5am:is (char-equal (cl-bnf:parse #'or-rule-with-function s) #\a)))
  (with-input-stream (s "-")
    (5am:is (equal (cl-bnf:parse #'or-rule-with-function s) nil))))

(cl-bnf:define-rule and-rule-with-functions
    (:and #'alpha-char-p #'numeric-char-p))

(5am:def-test test-and-rule-with-functions ()
  (with-input-stream (s "a1")
    (5am:is (equal (cl-bnf:parse #'and-rule-with-functions s) '(#\a #\1))))
  (with-input-stream (s "1b")
    (5am:is (equal (cl-bnf:parse #'and-rule-with-functions s) nil)))
  (with-input-stream (s "aa")
    (5am:is (equal (cl-bnf:parse #'and-rule-with-functions s) nil))))
