#|
  This file is a part of CL-Project project.
  Copyright (c) 2011 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-project-test
  (:use :cl
        :cl-project
        :prove))
(in-package :cl-project-test)

(plan 2)

(defvar *sample-project-directory*
    (asdf:system-relative-pathname
     :cl-project
     #p"t/cl-project-sample/"))

(uiop:delete-directory-tree *sample-project-directory* :validate t :if-does-not-exist :ignore)

(subtest "normal case"
  (cl-project:make-project *sample-project-directory*)
  (ok (uiop:directory-exists-p *sample-project-directory*)
      "Sample project was generated")
  (ok (asdf:load-system :cl-project-sample)
      "Can load the new project"))

(defvar *sample-caveman-directory*
  (asdf:system-relative-pathname
   :cl-project
   #P"t/cl-project-caveman/"))

(uiop:delete-directory-tree *sample-caveman-directory* :validate t :if-does-not-exist :ignore)

(subtest "Caveman2"
  (caveman2:make-project *sample-caveman-directory*)
  (ok (uiop:directory-exists-p *sample-caveman-directory*)
      "Sample project was generated")
  #+quicklisp
  (ok (ql:quickload :cl-project-caveman)
      "Can load the new project")
  #-quicklisp
  (skip 1 "Can load the new project"))

(finalize)
