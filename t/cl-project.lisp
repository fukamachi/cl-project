#|
  This file is a part of CL-Project project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-project-test
  (:use :cl
        :cl-project
        :cl-test-more)
  (:import-from :cl-fad
                :directory-exists-p
                :delete-directory-and-files))
(in-package :cl-project-test)

(plan 2)

(defvar *sample-project-directory*
    (asdf:system-relative-pathname
     :cl-project
     #p"t/cl-project-sample/"))

(when (cl-fad:file-exists-p *sample-project-directory*)
  (cl-fad:delete-directory-and-files *sample-project-directory*))

(cl-project:make-project *sample-project-directory*)

(ok (cl-fad:directory-exists-p *sample-project-directory*)
    "Sample project was generated")

(load (merge-pathnames "cl-project-sample.asd" *sample-project-directory*))

(ok (asdf:load-system (asdf:find-system :cl-project-sample))
    "Can load the new project")

(cl-fad:delete-directory-and-files *sample-project-directory*)

(finalize)
