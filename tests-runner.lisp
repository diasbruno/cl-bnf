(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)

(push *default-pathname-defaults* ql:*local-project-directories*)

(quicklisp:quickload :cl-bnf-tests)

(in-package #:cl-user)

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore c h))
        (uiop:quit -1))
      fiveam:*on-error* :debug)

(unless (fiveam:run-all-tests)
  (exit :code 1 :abort t))
