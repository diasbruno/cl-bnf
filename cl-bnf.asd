(asdf:defsystem #:cl-bnf
  :description "A simple BNF parser."
  :author "Bruno Dias <dias.h.bruno@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (#:utf8-input-stream
               #:flexi-streams)
  :components ((:module "src"
                :components ((:file "bnf")))))
