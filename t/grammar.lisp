(in-package :cl-bnf-tests)

(5am:def-suite test-grammar-suite
  :description "Test a full grammar.")

(5am:in-suite test-grammar-suite)

(define-rule single-character
    (:char . #\a))

(define-rule identifier
    (:* . single-character)
  :call (lambda (x)
	  (cons :identifier (coerce x 'string))))

(define-rule abc
    (:string . "abc"))

(define-rule repeat-abc
    (:* . abc))

(define-rule repeat-abc2
    (:* . (:or (:and abc
		     (:* . (:char . #\space)))
	       abc)))

(define-rule spaces
    (:* . (:char . #\space)))

(define-rule number-literal
    (:* . #'numeric-char-p)
  :call (lambda (value)
	  (cons :number (coerce value 'string))))

(define-grammar (language . kv)
  kv := identifier "-" number-literal
  :on (lambda (v) (list :assignment (car v) (caddr v))))

(5am:def-test test-composition ()
  (5am:is (equal (parse #'repeat-abc "abcabc")
		 '("abc" "abc"))))

(5am:def-test test-composition2 ()
  (5am:is (equal (parse #'repeat-abc2 "abc  abc")
		 '(("abc" (#\  #\ )) "abc"))))

(5am:def-test test-assignment ()
  (5am:is (equal (parse #'kv "a-1")
		 '(:assignment (:identifier . "a") (:number . "1")))))
