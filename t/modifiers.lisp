(in-package :cl-bnf-tests)

(5am:def-suite test-modifier-suite
  :description "Test BNF modifiers.")

(5am:in-suite test-modifier-suite)

(define-rule single-character
    (:char . #\a))

(5am:def-test test-single-char ()
  (with-utf8-input-stream (s "a")
   (5am:is (char-equal (parse #'single-character s) #\a)))
  (with-utf8-input-stream (s "b")
   (5am:is (null (parse #'single-character s)))))

(define-rule many-with-single-char
    (:* . (:char . #\a)))

(5am:def-test test-many-with-single-char ()
  (with-utf8-input-stream (s "aa")
   (5am:is (equal (parse #'many-with-single-char s) '(#\a #\a))))
  (with-utf8-input-stream (s "11")
   (5am:is (null (parse #'many-with-single-char s)))))

(define-rule many-using-declared-rule
    (:* . single-character))

(5am:def-test test-many-using-declared-rule ()
  (with-utf8-input-stream (s "aa")
    (5am:is (equal (parse #'many-using-declared-rule s) '(#\a #\a))))
  (with-utf8-input-stream (s "11")
    (5am:is (null (parse #'many-using-declared-rule s)))))
