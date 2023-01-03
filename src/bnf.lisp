(defpackage :cl-bnf
  (:use #:cl)
  (:export #:parse
           #:define-rule
           #:define-grammar
           #:string-match
           #:single-char
           #:maybe-match
           #:many-matches
           #:or-match
           #:and-match
           #:match-pred))

(in-package :cl-bnf)

(defmacro save-stream-position (body)
  `(let ((stream-position (slot-value stream 'SB-INT:INDEX)))
     ,body))

(defmacro consume-input (check success fail)
  `(save-stream-position
    (let ((current (read-char stream nil :eof)))
      (if ,check
          ,success
          (progn
            (setf (slot-value stream 'SB-INT:INDEX) stream-position)
            ,fail)))))

(defun match-pred (stream pred)
  "Get a single char from the STREAM and test against PRED."
  (consume-input
   (and (not (equal current :eof))
        (funcall pred current))
   (list :match stream current)
   (list :no-match stream)))

(defmacro single-char (stream char)
  "Get a single char from the STREAM and test against CHAR."
  `(match-pred ,stream #'(lambda (c) (char-equal ,char c))))

(defun string-match (stream str)
  "String pattern to be run on STREAM with STRING."
  (save-stream-position
   (let ((result (loop
                   :for c = (read-char stream nil :eof)
                   :for d :in (coerce str 'list)
                   :if (and (not (equal :eof c))
                            (char-equal c d))
                     :collect c)))
     (if (= (length str) (length result))
         (progn
           (setf (slot-value stream 'SB-INT:INDEX)
                 (1- (slot-value stream 'SB-INT:INDEX)))
           (list :match stream (concatenate 'string result)))
         (progn
           (setf (slot-value stream 'SB-INT:INDEX) stream-position)
           (list :no-match stream))))))

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
        (list :no-match stream))))

(defun or-match (stream expression)
  "Or pattern to be run on STREAM with EXPRESSION."
  (if (null expression)
      (list :no-match stream)
      (let* ((c (car expression))
             (result (eval-pattern-or-function c stream)))
        (if (equal :match (car result))
            result
            (or-match stream (cdr expression))))))

(defun and-match (stream expression)
  "And pattern to be run on STREAM with EXPRESSION."
  (let* ((cp-stream stream)
         (result (block nil
                   (loop
                     :for e :in expression
                     :as item = (eval-pattern-or-function e cp-stream)
                     :if (equal :match (car item))
                       :collect (progn
                                  (setf cp-stream (cadr item))
                                  (caddr item))
                     :else :do (return nil)))))
    (if result
        (list :match cp-stream result)
        (list :no-match stream))))

(defun ispair (x)
  (and (eql (type-of x) 'cons)
       (cdr x)
       (not (eql (type-of (cdr x)) 'cons))))

(defun evaluatable (item)
  "Check if ITEM is ready to be evaluated."
  (and (eql (type-of item) 'cons)
       (or (ispair item)
           (eql (type-of (car item)) 'keyword))))

(defun eval-pattern-or-function (item stream)
  "Evaluate a pattern or function for ITEM an use STREAM."
  (if (evaluatable item)
      (case (car item)
        (:char (single-char stream (cdr item)))
        (:string (string-match stream (cdr item)))
        (:? (maybe-match stream (cdr item)))
        (:* (many-matches stream (cdr item)))
        (:and (and-match stream (cdr item)))
        (:or (or-match stream (cdr item)))
        (t (error "meh")))
      (typecase item
        (cons (match-pred stream (cadr item)))
        (symbol (funcall item stream)))))

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
  "Parse according to the RULES on STREAM."
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
