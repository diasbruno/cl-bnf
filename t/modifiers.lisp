(in-package :cl-bnf-tests)

(5am:def-suite test-modifier-suite
  :description "Test BNF modifiers.")

(5am:in-suite test-modifier-suite)

(define-rule single-character
    (:char . #\a))

(5am:def-test test-single-char ()
  (5am:is (and
	   (char-equal (parse #'single-character "a") #\a)
	   (equal (parse #'single-character "b") nil))))

(define-rule many-with-single-char
    (:* . (:char . #\a)))

(5am:def-test test-many-with-single-char ()
  (5am:is (and
	   (equal (parse #'many-with-single-char "aa") '(#\a #\a))
	   (equal (parse #'many-with-single-char "11") 'nil))))

(define-rule many-using-declared-rule
    (:* . single-character))

(5am:def-test test-many-using-declared-rule ()
  (5am:is (and
	   (equal (parse #'many-using-declared-rule "aa") '(#\a #\a))
	   (equal (parse #'many-using-declared-rule "11") 'nil))))
