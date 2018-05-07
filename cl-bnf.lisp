(defun string-to-stream (string)
  "Parse from a STRING."
  (make-string-input-stream string
                            0
                            (length string)))

(defmacro one (stream pred)
  `(when (funcall ,pred (peek-char nil ,stream nil))
       (read-char ,stream nil nil)))

(defmacro many (stream pred)
  "Read from STREAM until PRED terminates the reading."
  `(concatenate 'string (loop
                           :for ch := (peek-char nil ,stream nil)
                           :while (and ch (funcall ,pred ch))
                           :collect (read-char ,stream nil nil))))

(defvar *expressions* nil)

; (declaim (inline numberic-char-p))
(defun numeric-char-p (char)
  (and (char-not-lessp char #\0)
       (char-not-greaterp char #\9)))

(defmacro := (label rule)
  (let ((kind (car rule))
        (expr (cadr rule)))
    `(defun ,label (source)
       (cond
         ((equal ,kind ':many) (many source ,expr))
         ((equal ,kind ':one) (one source ,expr))))))

(:= identifier
    (:many #'alpha-char-p))
(:= numberic
    (:many #'numeric-char-p))

(defun parse (source)
  (format nil "~a"
            (identifier source)))

(let ((text (string-to-stream "aa"))) (parse text))
