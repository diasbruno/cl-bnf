(in-package :cl-bnf-tests)

(5am:def-suite test-grammar-suite
  :description "Test a full grammar.")

(5am:in-suite test-grammar-suite)

(cl-bnf:define-rule single-character
    (:char . #\a))

(cl-bnf:define-rule identifier
    (:* . single-character)
  :call (lambda (x)
          (cons :identifier (coerce x 'string))))

(cl-bnf:define-rule abc
    (:string . "abc"))

(cl-bnf:define-rule repeat-abc
    (:* . abc))

(cl-bnf:define-rule repeat-abc2
    (:or (:and abc
               (:* . (:char . #\space))
               repeat-abc2)
         abc))

(cl-bnf:define-rule spaces
    (:* . (:char . #\space)))

(cl-bnf:define-rule number-literal
    (:* . #'numeric-char-p)
  :call (lambda (value)
          (cons :number (coerce value 'string))))

(cl-bnf:define-grammar (language . kv)
  kv := identifier "-" number-literal
  :on (lambda (v) (list :assignment (car v) (caddr v))))

(5am:def-test test-composition ()
  (with-input-stream (s "abcabc")
    (5am:is (equal (cl-bnf:parse #'repeat-abc s)
                   '("abc" "abc")))))

(5am:def-test test-composition2 ()
  (with-input-stream (s "abc  abc")
    (5am:is (equal (cl-bnf:parse #'repeat-abc2 s)
                   '("abc" (#\SPACE #\SPACE) "abc")))))

(5am:def-test test-composition3 ()
  (with-input-stream (s "abc")
    (5am:is (equal (cl-bnf:parse #'repeat-abc2 s)
                   "abc"))))

(5am:def-test test-assignment ()
  (with-input-stream (s "a-1")
    (5am:is (equal (cl-bnf:parse #'kv s)
                   '(:assignment (:identifier . "a") (:number . "1"))))))
