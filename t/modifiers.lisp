(in-package :cl-bnf-tests)

(5am:def-suite test-modifier-suite
  :description "Test BNF modifiers.")

(5am:in-suite test-modifier-suite)

(define-rule single-character
    (:char . #\a))

(5am:def-test test-single-char ()
  (let ((example1 (make-string-input-stream "a"))
	(example2 (make-string-input-stream "b")))
    (5am:is (and
	     (char-equal (parse #'single-character example1) #\a)
	     (equal (parse #'single-character example2) nil)))))

(define-rule many-with-single-char
    (:* . (:char . #\a)))

(5am:def-test test-many-with-single-char ()
  (let ((example1 (make-string-input-stream "aa"))
	(example2 (make-string-input-stream "11")))
    (5am:is (and
	     (equal (parse #'many-with-single-char example1) '(#\a #\a))
	     (equal (parse #'many-with-single-char example2) 'nil)))))

(define-rule many-using-declared-rule
    (:* . single-character))

(5am:def-test test-many-using-declared-rule ()
  (let ((example1 (make-string-input-stream "aa"))
	(example2 (make-string-input-stream "11")))
    (5am:is (and
	     (equal (parse #'many-using-declared-rule example1) '(#\a #\a))
	     (equal (parse #'many-using-declared-rule example2) 'nil)))))
