(defpackage :cl-bnf-json-example
  (:use #:cl #:cl-bnf)
  (:export #:run)
  (:import-from #:cl-bnf
                #:define-rule
                #:define-grammar))
(in-package :cl-bnf-json-example)

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
                                      "")))
                   vs :initial-value "")))
      ""))

(defun numeric-char-p (char)
  (and (char-not-lessp char #\0)
       (char-not-greaterp char #\9)))

(define-rule spaces (:many (:or (:char #\Space)
                                (:and (:char #\\)
                                      (:or (:char #\n)
                                           (:char #\t)))
                                (:char #\Newline)
                                (:char #\Tab)))
             :call #'stringify)

;; number

(define-rule decimal-number (:many (:one #'numeric-char-p)))
(define-rule real-number (:or (:and #'decimal-number
                                    (:char #\.)
                                    #'decimal-number)
                              (:and #'decimal-number
                                    (:char #\.))))
(define-rule signed-part (:or (:char #\+) (:char #\-)))
(define-rule exp-chars (:or (:char #\e)
                            (:char #\E)))
(define-rule exp-part (:or (:and #'exp-chars
                                 #'signed-part
                                 #'decimal-number)
                           (:and #'exp-chars
                                 #'decimal-number)))
(define-rule numeric (:or #'real-number
                          #'decimal-number))
(define-rule number-literal (:or (:and #'numeric
                                       #'exp-part)
                                 #'numeric)
             :call (lambda (matches)
                     (cons :number (stringify matches))))

;; string

(define-rule escaped-char (:and (:char #\\)
                                (:or (:char #\n)
                                     (:char #\t)
                                     (:char #\r)
                                     (:char #\b)
                                     (:char #\\)
                                     (:char #\/))))
(define-rule string-literal (:and (:char #\")
                                  (:many (:or #'escaped-char
                                              (:one #'alpha-char-p)))
                                  (:char #\"))
             :apply (lambda (a str b)
                      (declare (ignore a b))
                      (cons :string (stringify str))))

;; object

(define-rule key-value (:and #'string-literal
                             (:maybe #'spaces)
                             (:char #\:)
                             (:maybe #'spaces)
                             #'literals)
             :apply (lambda (key s c ss value)
                      (declare (ignore c s ss))
                      (cons key value)))
(define-rule key-value-continuation (:and #'key-value
                                          (:char #\,)
                                          (:maybe #'spaces))
             :apply (lambda (kv c s)
                      (declare (ignore c s))
                      kv))
(define-rule key-value-pairs (:or #'key-value-continuation
                                  #'key-value))
(define-rule filled-object (:and (:char #\{)
                                 (:many #'key-value-pairs)
                                 (:char #\}))
             :apply (lambda (a kvs b)
                      (declare (ignore a b))
                      `(:object ,kvs)))
(define-rule empty-object (:and (:char #\{)
                                (:maybe #'spaces)
                                (:char #\}))
             :apply (lambda (a b c)
                      (declare (ignore a b c))
                      `(:object)))
(define-rule object-literal (:or #'empty-object
                                 #'filled-object))

;; array

(define-rule empty-array (:and (:char #\[)
                               (:maybe #'spaces)
                               (:char #\]))
             :call (lambda (x)
                     (declare (ignore x))
                     '(:array)))
(define-rule array-item (:and (:maybe #'spaces)
                              (:many #'literals)
                              (:maybe #'spaces))
             :apply (lambda (s value ss)
                      (declare (ignore s ss))
                      value))
(define-rule array-continuation (:and #'array-item
                                      (:char #\,)
                                      (:maybe #'spaces))
             :apply (lambda (value c s)
                      (declare (ignore c s))
                      value))
(define-rule array-values (:or #'array-continuation
                               #'array-item))
(define-rule filled-array (:and (:char #\[)
                                (:many #'array-values)
                                (:char #\]))
             :apply (lambda (a values b)
                      (declare (ignore a b))
                      `(:array ,values)))
(define-rule array-literal (:or #'empty-array
                                #'filled-array))

(define-grammar json
  boolean-literal := (:or (:string "true")
                          (:string "false")) :call (lambda (value)
                          (cons :boolean (stringify value)))

  null-literal := (:string "null") :tag :null

  json := (:or #'array-literal
               #'object-literal
               #'string-literal
               #'number-literal
               #'boolean-literal
               #'null-literal))

(format t
        "~%~%Parsing a simple json file:~%~%~a~%"
        (parse #'literals "[{\"a\":1}\\n,true,false,null]"))
