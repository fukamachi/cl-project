#|
  This file is a part of CL-Project project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-project-test-asd
  (:use :cl :asdf))
(in-package :cl-project-test-asd)

(defsystem cl-project-test
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:cl-project
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "cl-project")))))
