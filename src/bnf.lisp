(defpackage :cl-bnf
  (:use #:cl)
  (:export #:parse
           #:define-rule
           #:define-grammar))

(in-package :cl-bnf)

(defstruct text-stream
  cursor
  text
  line
  column
  length)

(defun next-char (ts &key eof-char)
  "Look ahead the next char."
  (if (< (text-stream-cursor ts) (text-stream-length ts))
      (prog1 (char (text-stream-text ts)
                   (text-stream-cursor ts))
        (incf (text-stream-cursor ts)))
      eof-char))

(defun back-char (ts)
  "Move backward one char on TS."
  (setf (text-stream-cursor ts)
        (max (decf (text-stream-cursor ts)) 0)))

(defmacro one (stream pred)
  "Get a single char from the STREAM and test against PRED."
  `(let* ((cpy-stream (copy-text-stream ,stream))
          (current (next-char cpy-stream)))
     (if (and current (funcall ,pred current))
         (list :match cpy-stream current)
         (list :no-match ,stream))))

(defmacro single-char (stream char)
  "Get a single char from the STREAM and test against CHAR."
  `(let* ((cpy-stream (copy-text-stream ,stream))
          (current (next-char cpy-stream)))
     (if (and current (char-equal ,char current))
         (list :match cpy-stream current)
         (list :no-match ,stream))))

(defun string-match (stream string)
  "String pattern to be run on STREAM with STRING."
  (let* ((cp-stream (copy-text-stream stream))
         (result (loop
                   :for c = (next-char cp-stream :eof-char :eof)
                   :for d :in (coerce string 'list)
                   :if (and (not (equal :eof c)) (char-equal c d))
                     :collect c)))
    (if (= (length string) (length result))
        (progn
          (back-char cp-stream)
          (list :match cp-stream (concatenate 'string result)))
        (list :no-match stream))))

(defun maybe-match (stream expression)
  "Maybe pattern to be run on STREAM with EXPRESSION."
  (let ((result (eval-pattern-or-function expression stream)))
    (if (equal :no-match (car result))
        (list :match (cadr result) nil)
        result)))

(defun many-matches (stream expression)
  "Many pattern to be run on STREAM with EXPRESSION."
  (let* ((cp-stream stream)
         (result (loop
                   :as item = (eval-pattern-or-function expression cp-stream)
                   :while (equal :match (car item))
                   :collect (progn
                              (setf cp-stream (cadr item))
                              (caddr item)))))
    (if (> (length result) 0)
        (list :match cp-stream result)
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

(defun eval-pattern-or-function (item source)
  "Evaluate a patter or function for ITEM an use SOURCE."
  (case (type-of item)
    (symbol (funcall item source))
    (t (case (car item)
         (:char (single-char source (cdr item)))
         (:string (string-match source (cdr item)))
         (:maybe (maybe-match source (cdr item)))
         (:many (many-matches source (cdr item)))
         (:and (and-match source (cdr item)))
         (:or (or-match source (cdr item)))
         (t (one source (cadr item)))))))

(defmacro define-rule (label rule &key call tag apply)
  "Generate a function LABEL to parse RULE. Later,
you can apply a TRANSFORMATION which can be a function
or a keytword."
  `(defun ,label (source)
     (let ((result (eval-pattern-or-function ',rule source)))
       (if (equal :match (car result))
           (list :match (cadr result)
                 ,(cond
                    (call `(funcall ,call (nth 2 result)))
                    (apply `(apply ,apply (nth 2 result)))
                    (tag `(cons ,tag (nth 2 result)))
                    (t `(nth 2 result))))
           result))))

(defun map-rules (fn rules)
  (let ((index 0)
        (l (length rules))
        (rs nil))
    (do ((h (sequence:position := rules :start index)
            (sequence:position := rules :start index)))
        ((>= index l))
      (let* ((n (sequence:position := rules :start (1+ h)))
             (ss (subseq rules (- h 1) (or (and n (- n 1)) l))))
        (progn
          (push (funcall fn ss) rs)
          (setq index (or (and n (- n 1)) l)))))
    (values rs)))

(defun split-seq-on (item seq)
  (let ((index 0)
        (l (length seq))
        (rs nil))
    (do ((h (sequence:position item seq :start index)
            (sequence:position item seq :start index)))
        ((not h))
      (progn
        (setf rs (concatenate 'list rs (list (subseq seq index h))))
        (setq index (1+ h))))
    (append rs (list (subseq seq index l)))))

(defun process-rule (rules)
  (cons :or
        (mapcar (lambda (items)
                  (loop :for item :in items
                        :collect (etypecase item
                                   (standard-char (cons :char item))
                                   (string (cons :string item))
                                   (t item))))
                rules)))

(defmacro define-grammar (label &rest rules)
  `(eval-when (:compile-toplevel)
     ,@(map-rules (lambda (r)
                    (let* ((label (car r))
                           (rule-transform (split-seq-on :on (cddr r)))
                           (rule (split-seq-on :/ (car rule-transform)))
                           (transform (caadr rule-transform))
                           (pr (process-rule
                                (mapcar (lambda (r)
                                          (cons :and r))
                                        rule))))
                      `(define-rule ,label ,pr
                         :apply ,transform)))
                  rules)
     (defun ,label nil nil)))

(defun parse (rules source)
  "Parse according to the RULES on SOURCE."
  (let ((stream (make-text-stream :cursor 0
                                  :text source
                                  :line 0
                                  :column 0
                                  :length (length source))))
    (let ((result (funcall rules stream)))
      (caddr result))))
