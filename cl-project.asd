(defsystem "cl-project"
  :version "0.3.1"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("cl-emb"
               "uiop"
               "cl-ppcre"
               "local-time"
               "prove")
  :components ((:module "src"
                :components
                ((:file "cl-project" :depends-on ("specials" "file" "skeleton"))
                 (:file "file" :depends-on ("specials" "io"))
                 (:file "skeleton" :depends-on ("specials" "file" "middleware"))
                 (:file "io" :depends-on ("specials"))
                 (:file "middleware" :depends-on ("file"))
                 (:file "specials"))))
  :description "Generate a skeleton for modern project"
  :in-order-to ((test-op (test-op "cl-project-test"))))
