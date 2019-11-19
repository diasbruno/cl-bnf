(defpackage :cl-bnf-json-example
  (:use #:cl #:cl-bnf)
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

(define-grammar json

  null-literal := "null"
  :on (lambda (v) (declare (ignore v)) :null)

  boolean-literal := "true" :/ "false"
  :on (lambda (v) (cons :boolean v))

  space2 := #\Space :/ #\\ :/ #\n :/ #\t :/ #\Newline :/ #\Tab

  sp := (:* . space2)

  escaped-char := #\n :/ #\t :/ #\r :/ #\b :/ #\\ :/ #\/

  decimal-number := (:* . #'numeric-char-p)

  real-number := decimal-number #\. decimal-number :/ decimal-number #\.

  signed-part := #\+ :/ #\-

  exp-chars := #\e :/ #\E

  exp-part := exp-chars signed-part decimal-number :/ exp-chars decimal-number

  numeric := real-number :/ decimal-number

  number-literal := numeric exp-part :/ numeric
  :on (lambda (matches)
        (cons :number (stringify matches)))

  string-literal-contents := escaped-char :/ #'alpha-char-p

  string-literal := #\" (:* . string-literal-contents) #\"
  :on (lambda (str) (cons :string (nth 1 str)))

  array-continuation := literals (:? . sp) #\, (:? . sp) array-values
  :on (lambda (v) (list (car v) (nth 4 v)))

  array-values := array-continuation :/ literals

  array-literal := #\[ (:? . sp) (:? . (:* . array-values)) (:? . sp) #\]
  :on (lambda (v) (list :array (nth 2 v)))

  key-part := (:? . sp) string-literal (:? . sp)
  :on (lambda (v) (nth 1 v))

  key-value := key-part #\: (:? . sp) literals
  :on (lambda (v) (cons (car v) (nth 3 v)))

  kv-continuation := key-value (:? . sp) #\, (:? . sp) kv-pairs
  :on (lambda (v) (list (car v) (nth 4 v)))

  kv-pairs := kv-continuation :/ key-value

  object-literal := #\{ (:? . sp) (:? . (:* . kv-pairs)) (:? . sp) #\}
  :on (lambda (v)
        (list :object (nth 2 v)))

  literals := array-literal :/ object-literal :/ string-literal :/
  number-literal :/ boolean-literal :/ null-literal)

(print "results:")
(parse #'literals "null")
(parse #'boolean-literal "true")
(parse #'literals "1.0E+2")
(parse #'literals "[]")
(parse #'literals "[1,2]")
(parse #'literals "[ 1,2]")
(parse #'literals "[ 1 ,2]")
(parse #'literals "[ 1 , 2]")
(parse #'literals "[ 1 , 2 ]")
(parse #'literals "[ 1, 2 ]")
(parse #'literals "[1, 2 ]")
(parse #'literals "[1, 2 ,3 ]")
(parse #'literals "[1,]")
(parse #'literals "{}")
(parse #'literals "{\"a\":1}")
(parse #'literals "{  \"a\"  :  1  , \"b\"  :  2   }")
(parse #'literals "{\"a\"}")
(parse #'literals "{\"a\":}")
