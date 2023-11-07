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
