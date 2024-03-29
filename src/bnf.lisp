(defpackage :cl-bnf
  (:use #:cl)
  (:export #:parse
           #:define-rule
           #:define-grammar))

(in-package :cl-bnf)

(defmacro one (stream pred)
  "Get a single char from the STREAM and test against PRED."
  `(let* ((current (peek-char nil ,stream nil :eof)))
     (if (and (not (equal :eof current)) (funcall ,pred current))
         (prog1 (list :match ,stream current)
           (read-char ,stream nil nil))
         (progn
           (list :no-match ,stream)))))

(defmacro single-char (stream char)
  "Get a single char from the STREAM and test against CHAR."
  `(let* ((current (peek-char nil ,stream nil :eof)))
     (if (and (not (equal :eof current)) (char-equal ,char current))
         (prog1 (list :match ,stream current)
           (read-char ,stream nil nil))
         (progn
           (list :no-match ,stream)))))

(defun string-match (stream str)
  "String pattern to be run on STREAM with STRING."
  (let ((p (file-position stream))
        (result (loop
                  :for c = (peek-char nil stream nil :eof)
                  :for d :in (coerce str 'list)
                  :if (and (not (equal :eof c)) (char-equal c d))
                    :collect (prog1 c
                               (read-char stream nil :eof)))))
    (if (= (length str)
           (length result))
        (list :match stream (concatenate 'string result))
        (progn
          (file-position stream p)
          (list :no-match stream)))))

(defun maybe-match (stream expression)
  "Maybe pattern to be run on STREAM with EXPRESSION."
  (let ((result (eval-pattern-or-function expression stream)))
    (if (equal :no-match (car result))
        (list :match (cadr result) nil)
        result)))

(defun many-matches (stream expression)
  "Many pattern to be run on STREAM with EXPRESSION."
  (let ((result (loop
                  :as item = (eval-pattern-or-function expression stream)
                  :while (equal :match (car item))
                  :collect (caddr item))))
    (if (> (length result) 0)
        (list :match stream result)
        (progn
          (list :no-match stream)))))

(defun or-match (stream expression)
  "Or pattern to be run on STREAM with EXPRESSION."
  (if (null expression)
      (progn
        (list :no-match stream))
      (let* ((c (car expression))
             (result (eval-pattern-or-function c stream)))
        (if (equal :match (car result))
            result
            (or-match stream (cdr expression))))))

(defun and-match (stream expression)
  "And pattern to be run on STREAM with EXPRESSION."
  (let ((p (file-position stream))
        (result (block nil
                  (loop
                    :for e :in expression
                    :as item = (eval-pattern-or-function e stream)
                    :if (progn
                          (equal :match (car item)))
                      :collect (caddr item)
                    :else :do (return nil)))))
    (if result
        (list :match stream result)
        (progn
          (file-position stream p)
          (list :no-match stream)))))

(defun ispair (x)
  (and (eql (type-of x) 'cons)
       (cdr x)
       (not (eql (type-of (cdr x)) 'cons))))

(defun eval-pattern-or-function (item source)
  "Evaluate a patter or function for ITEM an use SOURCE."
  (if (and (eql (type-of item) 'cons)
         (or (ispair item)
            (eql (type-of (car item)) 'keyword)))
      (let ((r (case (car item)
                 (:char (single-char source (cdr item)))
                 (:string (string-match source (cdr item)))
                 (:? (maybe-match source (cdr item)))
                 (:* (many-matches source (cdr item)))
                 (:and (and-match source (cdr item)))
                 (:or (or-match source (cdr item)))
                 (t (error "meh")))))
        r)
      (typecase item
        (cons (one source (cadr item)))
        (symbol (funcall item source)))))

(defmacro define-rule (label rule &key call tag apply)
  "Generate a function LABEL to parse RULE. Later,
you can apply a TRANSFORMATION which can be a function
or a keytword."
  `(defun ,label (stream)
     (let ((result (eval-pattern-or-function ',rule stream)))
       (if (equal :match (car result))
           (list :match (cadr result)
                 ,(cond
                    (call `(funcall ,call (nth 2 result)))
                    (apply `(apply ,apply (nth 2 result)))
                    (tag `(cons ,tag (nth 2 result)))
                    (t `(nth 2 result))))
           result))))

(defun map-rules (fn rules)
  "Apply FN on each expression in RULES."
  (let ((index 0) (l (length rules)) (rs nil))
    (do ((h (position := rules :start index)
            (position := rules :start index)))
        ((>= index l))
      (let* ((n (position := rules :start (1+ h)))
             (ss (subseq rules (- h 1) (or (and n (- n 1)) l))))
        (progn
          (push (funcall fn ss) rs)
          (setq index (or (and n (- n 1)) l)))))
    (values rs)))

(defun split-seq-on (item seq)
  "Split on ITEM on SEQ."
  (let ((index 0) (l (length seq)) (rs nil))
    (do ((h (position item seq :start index)
            (position item seq :start index)))
        ((not h))
      (progn
        (setf rs (concatenate 'list rs (list (subseq seq index h))))
        (setq index (1+ h))))
    (append rs (list (subseq seq index l)))))

(defun expand-item (item)
  "If ITEM is simple type, transforms it into the correct structure."
  (etypecase item
    (standard-char (cons :char item))
    (base-char (cons :char item))
    (string (cons :string item))
    (symbol item)
    (function item)
    (t (if (or (ispair item)
               (eql (type-of (cadr item)) 'keyword))
           (cons (car item) (expand-item (cdr item)))
           (mapcar #'expand-item item)))))

(defun expand-and-rule (items)
  "Take many or RULES and normalize the tree."
  (let ((e (expand-item items)))
    (if (> (length items) 1)
        (cons :and e)
        (car e))))

(defun parse (rules stream)
  "Parse according to the RULES on SOURCE."
  (caddr (funcall rules stream)))

(defmacro define-grammar (spec &rest rules)
  "Generates the parser with SPEC and all the RULES."
  `(progn
     ,@(map-rules (lambda (r)
                    (let* ((label (car r))
                           ;; rule transform => (rule, trasform)
                           (rule-transform (split-seq-on :on (cddr r)))
                           ;; or cases => rule :: [[r] [r]]
                           (rule (split-seq-on :/ (car rule-transform)))
                           ;; trasform :: args -> a
                           (transform (caadr rule-transform))
                           (pr (let ((ex (mapcar #'expand-and-rule rule)))
                                 (if (> (length ex) 1)
                                     (cons :or ex)
                                     (car ex)))))
                      `(define-rule ,label ,pr :call ,transform)))
                  rules)
     (defun ,(car spec) (stream)
       (parse (function ,(cdr spec)) stream))))
