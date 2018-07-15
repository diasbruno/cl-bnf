(asdf:defsystem #:cl-bnf-examples
  :description "Run all examples."
  :author "Bruno Dias <dias.h.bruno@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (#:cl-bnf)
  :components ((:module "example"
                :components ((:file "json-parse")))))
