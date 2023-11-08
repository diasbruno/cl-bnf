(in-package :utf8-input-stream)

(defmethod stream-unread-char ((s character-input-stream) ch)
  (let* ((b0 (char-code ch))
         (give-back (cond
                      ((one-byte-ch? b0) 1)
                      ((two-bytes-ch? b0) 2)
                      ((three-bytes-ch? b0) 3)
                      ((four-bytes-ch? b0) 4))))
    (setf (stream-context-pos (ctx s))
          (- (stream-context-pos (ctx s)) give-back))
    (setf (stream-context-buf-pos (ctx s))
          (- (stream-context-buf-pos (ctx s)) give-back))))

(in-package :cl-user)

(defpackage #:cl-bnf-tests
  (:use #:cl #:fiveam #:cl-bnf)
  (:import-from #:cl-bnf
                #:define-rule))

(in-package :cl-bnf-tests)

(defmacro with-utf8-input-stream ((var source) &body body)
  `(let ((,var (utf8-input-stream:make-utf8-input-stream
                (flex:make-in-memory-input-stream
                 (flex:string-to-octets ,source :external-format
                                        (flex:make-external-format :utf-8))))))
     ,@body))
