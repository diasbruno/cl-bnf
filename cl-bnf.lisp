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

(defun string-match (stream str)
  "Check if the next word in TS is in STR."
  (let* ((cp-stream (copy-text-stream stream))
         (result (loop
                    :for c = (next-char cp-stream :eof-char :eof)
                    :for d :in (coerce str 'list)
                    :if (and (not (equal :eof c)) (char-equal c d))
                    :collect c)))
    (format t "Match string result ~a stream ~a.~%" result cp-stream)
    (if (= (length str) (length result))
        (progn
          (back-char cp-stream)
          (list :match cp-stream result))
        (list :no-match stream))))

(defun string-to-stream (string)
  "Parse from a STRING."
  (make-string-input-stream string 0 (length string)))

(defun numeric-char-p (char)
  (and (char-not-lessp char #\0)
       (char-not-greaterp char #\9)))

(defmacro one (stream pred)
  `(progn
     (format t "Running one with ~a stream ~a.~%" ,pred ,stream)
     (let* ((cpy-stream (copy-text-stream ,stream))
            (current (next-char cpy-stream)))
       (if (and current (funcall ,pred current))
           (progn
             (format t "Result of one ~a stream ~a~%" current ,stream)
             (list :match cpy-stream current))
           (list :no-match ,stream)))))

(defmacro single-char (stream ch)
  `(progn
     (format t "Runnig single-char with ~a stream ~a~%" ,ch ,stream)
     (let* ((cpy-stream (copy-text-stream ,stream))
            (current (next-char cpy-stream)))
       (if (and current (char-equal ,ch current))
           (progn
             (format t "Result of single-char ~a.~%" current)
             (list :match cpy-stream current))
           (list :no-match ,stream)))))

(defun many (stream expr)
  "Read from STREAM until EXPR terminates the reading."
  (progn
    (format t "Runnig many with ~a stream ~a~%" expr stream)
    (let* ((cp-stream stream)
           (result (loop
                      :as item = (eval-pattern-or-function expr cp-stream)
                      :while (equal :match (car item))
                      :collect (progn
                                 (setf cp-stream (cadr item))
                                 (caddr item)))))
      (format t "Many result ~a." result)
      (if (> (length result) 0)
          (list :match cp-stream result)
          (list :no-match stream)))))

(defun or-match (stream expr)
  (format t "Running or with ~a stream ~a~%" expr stream)
  (when (not (null expr))
    (let* ((c (car expr))
           (result (eval-pattern-or-function c stream)))
      (format t "Or result of ~a: ~a~%" c result)
      (if (equal :match (car result))
          result
          (or-match stream (cdr expr))))))

(defun and-match (stream expr)
  (format t "Running and with ~a stream ~a~%" expr stream)
  (let* ((cp-stream stream)
         (result (block nil
                   (loop
                      :for e :in expr
                      :as item = (eval-pattern-or-function e cp-stream)
                      :if (equal :match (car item))
                      :collect (progn
                                 (setf cp-stream (cadr item))
                                 (caddr item))
                      :else :do (progn
                                  (format t "Bailing out ~a.~%" e)
                                  (return nil))))))
    (format t "And Result ~a stream ~a~%" result cp-stream)
    (if result
        (list :match cp-stream result)
        (list :no-match stream))))

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
          (:string (string-match source (cadr item)))
          (:many (many source (cadr item)))
          (:and (and-match source (cdr item)))
          (:or (or-match source (cdr item)))))))

(defmacro := (label rule)
  `(defun ,label (source)
     (eval-pattern-or-function ',rule source)))

(defun parse (rules source)
  (let ((stream (make-text-stream :cursor 0
                                  :text source
                                  :line 0
                                  :column 0
                                  :length (length source))))
    (let ((result (funcall rules stream)))
      (caddr result))))
