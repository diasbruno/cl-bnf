(defpackage #:cl-bnf-tests
  (:use #:cl #:fiveam #:cl-bnf)
  (:import-from #:cl-bnf
                #:define-rule))

(in-package :cl-bnf-tests)

(5am:def-suite test-suite
    :description "Suite for tests which should fail.")

(defun stringify (vs)
    (if vs
        (case (type-of vs)
          (standard-char (string vs))
          (t (reduce (lambda (acc x)
                       (concatenate 'string acc
                                    (if x
                                        (case (type-of x)
                                          (standard-char (string x))
                                          (list (stringify x))
                                          (cons (stringify x)))
                                        ""))) vs :initial-value "")))
        ""))

(defun numeric-char-p (char)
    (and (char-not-lessp char #\0)
         (char-not-greaterp char #\9)))

(define-rule single-character (:char #\a))
(define-rule one-character (:one #'alpha-char-p))
(define-rule many-a (:many (:char #\a)))
(define-rule wording (:many #'single-character))
(define-rule ident-or-num (:or (:one #'alpha-char-p)
                               (:one #'numeric-char-p)))
(define-rule letter-num (:and (:one #'alpha-char-p)
                              (:one #'numeric-char-p)))

(define-rule identifier (:many #'single-character)
             :call (lambda (x)
                     (cons :identifier (stringify x))))

(define-rule abc (:string "abc"))
(define-rule repeat-abc (:many #'abc))

(define-rule repeat-abc2 (:many (:or (:and #'abc
                                           (:many (:char #\space)))
                                     #'abc)))

(define-rule spaces (:many (:char #\space)))

(define-rule number-literal (:many (:one #'numeric-char-p))
             :call (lambda (value) (cons :number (stringify value))))
(define-grammar assignment
    assignment := (:and #'identifier
                        (:maybe #'spaces)
                        (:char #\=)
                        (:maybe #'spaces)
                        #'number-literal) :apply (lambda (lhs sp e sp2 expr)
                                                   (declare (ignore e sp sp2))
                                                   `(:assignment ,lhs ,expr)))

(5am:def-test test-single-char (:suite test-suite)
  (5am:is (char-equal (parse #'single-character "a") #\a)))

(5am:def-test test-one-char (:suite test-suite)
  (5am:is (char-equal (parse #'one-character "a") #\a)))

(5am:def-test test-many (:suite test-suite)
  (5am:is (equal (parse #'many-a "aa") '(#\a #\a))))

(5am:def-test test-fail-many (:suite test-suite)
  (5am:is (equal (parse #'many-a "11") 'nil)))

(5am:def-test test-wording (:suite test-suite)
  (5am:is (equal (parse #'wording "aa") '(#\a #\a))))

(5am:def-test test-or-match (:suite test-suite)
  (5am:is (char-equal (parse #'ident-or-num "1") #\1)))

(5am:def-test test-and-match (:suite test-suite)
  (5am:is (equal (parse #'letter-num "a1") '(#\a #\1))))

(5am:def-test test-and-fail-match (:suite test-suite)
  (5am:is (equal (parse #'letter-num "aa") nil)))

(5am:def-test test-composition (:suite test-suite)
  (5am:is (equal (parse #'repeat-abc "abcabc")
                 '((#\a #\b #\c) (#\a #\b #\c)))))

(5am:def-test test-composition2 (:suite test-suite)
  (5am:is (equal (parse #'repeat-abc2 "abc  abc")
                 '(((#\a #\b #\c) (#\space #\space)) (#\a #\b #\c)))))

(5am:def-test test-assignment (:suite test-suite)
  (5am:is (equal (parse #'assignment "a=1")
                 '(:assignment (:identifier . "a")
                   (:number . "1")))))
