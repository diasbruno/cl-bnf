(asdf:defsystem #:cl-bnf
  :description "A simple BNF parser."
  :author "Bruno Dias <dias.h.bruno@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:module "src/"
                         :serial t
                         :components ((:file "bnf")))))
