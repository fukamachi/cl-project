#|
  This file is a part of CL-Project package.
  URL: http://github.com/fukamachi/cl-project
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  CL-Project is freely distributable under the LLGPL License.
|#

#|
  Generate a skeleton for modern project

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(defsystem "cl-project"
  :version "0.3.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
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
