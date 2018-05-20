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
(:= repeat-abc (:many (:and (:char #\a)
                            (:char #\b)
                            (:char #\c))))

(5am:def-test test-single-char (:suite test-suite)
  (5am:is (char-equal (parse (string-to-stream "a") #'single-character)
                      #\a)))

(5am:def-test test-one-char (:suite test-suite)
  (5am:is (char-equal (parse (string-to-stream "a") #'one-character)
                      #\a)))

(5am:def-test test-many (:suite test-suite)
  (5am:is (equal (parse (string-to-stream "aa") #'many-a)
                 '(#\a #\a))))

(5am:def-test test-wording (:suite test-suite)
  (5am:is (equal (parse (string-to-stream "aa") #'wording)
                 '(#\a #\a))))

(5am:def-test test-or-match (:suite test-suite)
  (5am:is (char-equal (parse (string-to-stream "1") #'ident-or-num)
                      #\1)))

(5am:def-test test-and-match (:suite test-suite)
  (5am:is (equal (parse (string-to-stream "a1") #'letter-num)
                 '(#\a #\1))))

(5am:def-test test-composition (:suite test-suite)
  (5am:is (equal (parse (string-to-stream "abcabc") #'repeat-abc)
                 '((#\a #\b #\c) (#\a #\b #\c)))))


;; (defvar *ts* nil)
;; (setq *ts* (make-text-stream :cursor 0
;; :text "asdf"
;; :line 0
;; :column 0
;; :length (length "asdf")))

;; (let ((text (string-to-stream "a()")))
;;   (parse text #'an))

(5am:run-all-tests)
