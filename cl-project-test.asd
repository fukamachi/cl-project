#|
  This file is a part of CL-Project project.
  Copyright (c) 2011 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(defsystem "cl-project-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("cl-project"
               "prove"
               "caveman2"
               "uiop")
  :components ((:module "tests"
                :components
                ((:test-file "cl-project"))))
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
