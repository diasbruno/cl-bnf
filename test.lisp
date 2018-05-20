(load #P"~/projects/cl-bnf/cl-bnf.lisp")
(quicklisp:quickload 'fiveam)

(5am:def-suite test-suite :description "Suite for tests which should fail.")

(:= single-character (:char #\a))
(:= one-character (:one #'alpha-char-p))
(:= many-a (:many (:char #\a)))
(:= wording (:many #'single-character))
(:= ident-or-num (:or (:one #'alpha-char-p)
                      (:one #'numeric-char-p)))
(:= letter-num (:and (:one #'alpha-char-p)
                     (:one #'numeric-char-p)))

(:= abc (:string "abc"))
(:= repeat-abc (:many #'abc))

(:= repeat-abc2 (:many (:or (:and #'abc
                                  (:many (:char #\space)))
                            #'abc)))
(:= spaces (:many (:char #\space)))
(:= numeric (:many (:one #'numeric-char-p)))
(:= assignment (:and #'wording
                     (:maybe #'spaces)
                     (:char #\=)
                     (:maybe #'spaces)
                     #'numeric))

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
                 '((#\a)
                   nil
                   #\=
                   nil
                   (#\1)))))

(5am:run-all-tests)
