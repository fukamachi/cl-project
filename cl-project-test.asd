#|
  This file is a part of CL-Project project.
  Copyright (c) 2011 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-project-test-asd
  (:use :cl :asdf))
(in-package :cl-project-test-asd)

(defsystem cl-project-test
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :defsystem-depends-on (:prove-asdf)
  :depends-on (:cl-project
               :prove
               :uiop)
  :components ((:module "t"
                :components
                ((:test-file "cl-project"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove.asdf) c)))
