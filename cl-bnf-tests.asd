(asdf:defsystem #:cl-bnf-tests
  :description "Tests for cl-bnf."
  :author "Bruno Dias <dias.h.bruno@gmail.com>"
  :license "MIT"
  :depends-on (#:cl-bnf #:fiveam)
  :serial t
  :components ((:module "t"
                :components ((:file "tests")))))
