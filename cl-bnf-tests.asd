(asdf:defsystem #:cl-bnf-tests
  :description "Tests for cl-bnf."
  :author "Bruno Dias <dias.h.bruno@gmail.com>"
  :license "MIT"
  :depends-on (#:fiveam
               #:cl-bnf)
  :serial t
  :components ((:module "t"
                :components ((:file "package")
                             (:file "common")
                             (:file "modifiers")
                             (:file "rules")
                             (:file "grammar")))))
