(defpackage #:cl-bnf-tests
  (:use #:cl #:fiveam #:cl-bnf)
  (:import-from #:cl-bnf
                #:define-rule))

(in-package :cl-bnf-tests)

(5am:def-suite test-suite
  :description "Suite for tests which should fail.")

(defun numeric-char-p (char)
  (and (char-not-lessp char #\0)
       (char-not-greaterp char #\9)))

(define-rule single-character
    (:char . #\a))

(define-rule many-a
    (:many . (:char . #\a)))

(define-rule wording
    (:many . single-character))

(define-rule letter-or-num
    (:or #'alpha-char-p
         #'numeric-char-p))

(define-rule letter-num
    (:and #'alpha-char-p
          #'numeric-char-p))

(define-rule identifier
    (:many . single-character)
  :call (lambda (x)
          (cons :identifier (coerce x 'string))))

(define-rule abc
    (:string . "abc"))

(define-rule repeat-abc
    (:many . abc))

(define-rule repeat-abc2
    (:many . (:or (:and abc
                        (:many . (:char . #\space)))
                  abc)))

(define-rule spaces
    (:many . (:char . #\space)))

(define-rule number-literal
    (:many . #'numeric-char-p)
  :call (lambda (value)
          (cons :number (coerce value 'string))))

(define-grammar language
  kv := identifier "-" number-literal
  :on (lambda (lhs dash expr)
        (list :assignment lhs expr)))

(5am:def-test test-single-char (:suite test-suite)
  (5am:is (char-equal (parse #'single-character "a") #\a)))

(5am:def-test test-many (:suite test-suite)
  (5am:is (equal (parse #'many-a "aa") '(#\a #\a))))

(5am:def-test test-fail-many (:suite test-suite)
  (5am:is (equal (parse #'many-a "11") 'nil)))

(5am:def-test test-wording (:suite test-suite)
  (5am:is (equal (parse #'wording "aa") '(#\a #\a))))

(5am:def-test test-or-match (:suite test-suite)
  (5am:is (char-equal (parse #'letter-or-num "1") #\1)))

(5am:def-test test-and-match (:suite test-suite)
  (5am:is (equal (parse #'letter-num "a1") '(#\a #\1))))

(5am:def-test test-and-fail-match (:suite test-suite)
  (5am:is (equal (parse #'letter-num "aa") nil)))

(5am:def-test test-composition (:suite test-suite)
  (5am:is (equal (parse #'repeat-abc "abcabc")
                 '("abc" "abc"))))

(5am:def-test test-composition2 (:suite test-suite)
  (5am:is (equal (parse #'repeat-abc2 "abc  abc")
                 '(("abc" (#\  #\ )) "abc"))))

(5am:def-test test-assignment (:suite test-suite)
  (5am:is (equal (parse #'kv "a-1")
                 '(:assignment (:identifier . "a") (:number . "1")))))
