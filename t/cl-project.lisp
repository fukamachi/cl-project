#|
  This file is a part of CL-Project project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-project-test
  (:use :cl
        :cl-project
        :cl-test-more)
  (:import-from :osicat
				:file-exists-p
				:directory-exists-p
				:delete-directory-and-files)
  (:import-from :inferior-shell :run/s))
(in-package :cl-project-test)

(plan 3)

(defvar *sample-project-directory*
    (asdf:system-relative-pathname
     :cl-project
     #p"t/cl-project-sample/"))

(delete-directory-and-files 
 *sample-project-directory*
 :if-does-not-exist :ignore)

(cl-project:make-project *sample-project-directory* :git t)

(ok (directory-exists-p *sample-project-directory*)
    "Sample project was generated")

(ok (asdf:load-system :cl-project-sample)
    "Can load the new project")

(ok (directory-exists-p 
	 (merge-pathnames
	  *sample-project-directory* ".git/"))
    "git repos initialized")
(delete-directory-and-files *sample-project-directory*)

(finalize)
