(in-package :cl-bnf-tests)

(5am:def-suite test-rule-suite
  :description "Test for rules.")

(5am:in-suite test-rule-suite)

(define-rule or-rule-with-function
    (:or #'alpha-char-p #'numeric-char-p))

(5am:def-test test-or-rule-with-function ()
  (with-utf8-input-stream (s "1")
    (5am:is (char-equal (parse #'or-rule-with-function s) #\1)))
  (with-utf8-input-stream (s "a")
    (5am:is (char-equal (parse #'or-rule-with-function s) #\a)))
  (with-utf8-input-stream (s "-")
    (5am:is (equal (parse #'or-rule-with-function s) nil))))

(define-rule and-rule-with-functions
    (:and #'alpha-char-p #'numeric-char-p))

(5am:def-test test-and-rule-with-functions ()
  (with-utf8-input-stream (s "a1")
    (5am:is (equal (parse #'and-rule-with-functions s) '(#\a #\1))))
  (with-utf8-input-stream (s "1b")
    (5am:is (equal (parse #'and-rule-with-functions s) nil))))
