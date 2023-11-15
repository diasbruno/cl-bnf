(in-package :cl-user)

(defpackage #:cl-bnf-tests
  (:use #:cl)
  (:import-from #:cl-bnf
                #:define-rule))

(in-package :cl-bnf-tests)

(defmacro with-input-stream ((var source) &body body)
  `(let ((,var (make-string-input-stream ,source)))
     ,@body))
