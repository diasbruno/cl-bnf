(defun string-to-stream (string)
  "Parse from a STRING."
  (make-string-input-stream string
                            0
                            (length string)))

(defun numeric-char-p (char)
  (and (char-not-lessp char #\0)
       (char-not-greaterp char #\9)))

(defmacro single-char (stream ch)
  `(when (char-equal ,ch (peek-char nil ,stream nil))
       (string (read-char ,stream nil nil))))

(defmacro one (stream pred)
  `(when (funcall ,pred (peek-char nil ,stream nil))
       (string (read-char ,stream nil nil))))

(defmacro many (stream pred)
  "Read from STREAM until PRED terminates the reading."
  `(concatenate 'string (loop
                           :for ch := (peek-char nil ,stream nil)
                           :while (and ch (funcall ,pred ch))
                           :collect (read-char ,stream nil nil))))

(defun or-match (source expr)
  (when (not (null expr))
   (let* ((c (car expr))
          (result (funcall c source)))
     (if (> (length result) 0)
         result
         (or-match source (cdr expr))))))

(defun and-match (source expr)
  (if (null expr)
      nil
      (let* ((c (car expr))
             (result (funcall c source)))
        (if (> (length result) 0)
            (list result (and-match source (cdr expr)))
            nil))))

(defmacro := (label rule)
  (let ((kind (car rule))
        (rules (cdr rule))
        (expr (cadr rule)))
    `(defun ,label (source)
       ,(cond
          ((equal kind ':char) `(single-char source ,expr))
          ((equal kind ':one) `(one source ,expr))
          ((equal kind ':many) `(many source ,expr))
          ((equal kind ':or) `(or-match source (list ,@rules)))
          ((equal kind ':and) `(and-match source (list ,@rules)))))))

(:= identifier
    (:many #'alpha-char-p))
(:= numeric
    (:many #'numeric-char-p))
(:= ident-or-num
    (:or #'identifier #'numeric))
(:= an
    (:and #'identifier #'numeric #'ident-or-num))

(:= open-parens (:char #\())
(:= close-parens (:char #\)))
(:= fcall (:and #'identifier #'open-parens #'close-parens))

(defun parse (source rules)
  (funcall rules source))

(let ((text (string-to-stream "a()")))
  (parse text #'))
