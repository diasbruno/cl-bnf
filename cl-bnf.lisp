;;  - Leaf
;; :char
;; :string
;;
;; - Nodes
;; :or
;; :and
;; :* = 0 or many
;; :? = maybe one
;; :+ = many
;;
;; - Idea
;; (match :* :one-of "abc")
;; (match :* :char "abc")
;; (match :* :char "abc")

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

(defun match-char (ts ch)
  (let ((c (next-char ts :eof-char :eof)))
    (if (char-equal c ch)
        (values :match c ts)
        (progn
          (back-char ts)
          (values :no-match ts)))))

(defun match-string (ts str)
  "Check if the next word in TS is in STR."
  (let ((stopped nil)
        (word nil)
        (ts2 (copy-text-stream ts)))
    (loop :for c = (next-char ts2 :eof-char :eof)
          :for d :in (coerce str 'list)
          :if (and (not (equal :eof c)) (char-equal c d))
          :do (setf word (concatenate 'string word (string c)))
          :else :do (setf stopped t)
          :finally (if stopped
                       (return (values :match ts2 word))
                     (return (values :no-match ts))))))

(defun string-to-stream (string)
  "Parse from a STRING."
  (make-string-input-stream string 0 (length string)))

(defun numeric-char-p (char)
  (and (char-not-lessp char #\0)
       (char-not-greaterp char #\9)))

(defmacro one (stream pred)
  `(when (funcall ,pred (peek-char nil ,stream nil))
     (read-char ,stream nil nil)))

(defmacro single-char (stream ch)
  `(progn
     (format t "Runnig single-char with ~a~%" ,ch)
     (let ((c (peek-char nil ,stream nil)))
       (when (and c (char-equal ,ch c))
         (read-char ,stream nil nil)))))

(defun many (source expr)
  "Read from STREAM until EXPR terminates the reading."
  (progn
    (format t "Runnig many with ~a~%" expr)
    (loop :as item = (eval-pattern-or-function expr source)
       :while item
       :collect item)))

(defun or-match (source expr)
   (format t "Running or with ~a ~%" expr)
   (when (not (null expr))
     (let* ((c (car expr))
            (result (eval-pattern-or-function c source)))
       (format t "Or result of ~a: ~a~%" c result)
       (if result
           result
           (or-match source (cdr expr))))))

(defun and-match (source expr)
  (format t "Running and with ~a ~%" expr)
  (unless (null expr)
    (let* ((c (car expr))
           (result (eval-pattern-or-function c source)))
      (when result
        (format t "And result of ~a: ~a~%" c result)
        (cons result (and-match source (cdr expr)))))))

(defun eval-pattern-or-function (item source)
  (format t "Evaluating ~a is ~a~%" item (type-of item))
  (if (eql (type-of item) 'function)
      (prog1 (funcall item source)
        (format t "Applying function ~a~%" item))
    (progn
      (format t "Expression ~a is ~a, ~a is ~a~%"
              (car item) (type-of (car item))
              (cdr item) (type-of (cdr item)))
      (case (car item)
         ('function (funcall (cadr item) source))
         (:char (single-char source (cadr item)))
         (:one (one source (cadr (cadr item))))
         (:many (many source (cadr item)))
         (:and (and-match source (cdr item)))
         (:or (or-match source (cdr item)))))))

(defmacro := (label rule)
  `(defun ,label (source)
     (eval-pattern-or-function ',rule source)))

(defun parse (source rules)
  (funcall rules source))
