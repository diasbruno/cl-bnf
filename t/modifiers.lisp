(in-package :cl-bnf-tests)

(5am:def-suite test-modifier-suite
  :description "Test BNF modifiers.")

(5am:in-suite test-modifier-suite)

(cl-bnf:define-rule single-character
    (:char . #\a))

(5am:def-test test-single-char ()
  (with-input-stream (s "a")
   (5am:is (char-equal (cl-bnf:parse #'single-character s) #\a)))
  (with-input-stream (s "b")
   (5am:is (null (cl-bnf:parse #'single-character s)))))

(cl-bnf:define-rule many-with-single-char
    (:* . (:char . #\a)))

(5am:def-test test-many-with-single-char ()
  (with-input-stream (s "aa")
   (5am:is (equal (cl-bnf:parse #'many-with-single-char s) '(#\a #\a))))
  (with-input-stream (s "11")
   (5am:is (null (cl-bnf:parse #'many-with-single-char s)))))

(cl-bnf:define-rule many-using-declared-rule
    (:* . single-character))

(5am:def-test test-many-using-declared-rule ()
  (with-input-stream (s "aa")
    (5am:is (equal (cl-bnf:parse #'many-using-declared-rule s) '(#\a #\a))))
  (with-input-stream (s "11")
    (5am:is (null (cl-bnf:parse #'many-using-declared-rule s)))))
