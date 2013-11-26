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
                :file-exists-p
                :directory-exists-p
                :delete-directory-and-files))
(in-package :cl-project-test)


(defvar *sample-project-directory*)

(let ((*sample-project-directory*
       (asdf:system-relative-pathname
        :cl-project
        #p"t/cl-project-sample/")))
  (unwind-protect
       (progn
         (plan 2)
         (when (file-exists-p *sample-project-directory*)
           (delete-directory-and-files *sample-project-directory*))
         (cl-project:make-project *sample-project-directory*)
         (ok (directory-exists-p *sample-project-directory*)
             "Sample project was generated")
         (ok (asdf:load-system :cl-project-sample)
             "Can load the new project")
         (finalize))
    (delete-directory-and-files *sample-project-directory*)))

(let ((*sample-project-directory*
       (asdf:system-relative-pathname
        :cl-project
        #p"t/cl-project-sample-mod/")))
  (unwind-protect
       (progn
         (plan 2)
         (cl-project:make-project *sample-project-directory*
                                  :processor 'modified-processor
                                  :author "alien tech")
         (ok (directory-exists-p *sample-project-directory*)
             "Sample project was generated")
         (ok (asdf:load-system :cl-project-sample)
             "Can load the new project")
         (finalize))
    (delete-directory-and-files *sample-project-directory*)))

(let ((*sample-project-directory*
       (asdf:system-relative-pathname
        :cl-project
        #p"t/cl-project-sample-eos/")))
  (unwind-protect
       (progn
         (plan 2)
         (cl-project:make-project *sample-project-directory*
                                  :processor 'optima-like-processor)
         (ok (directory-exists-p *sample-project-directory*)
             "Sample project was generated")
         (ok (asdf:load-system :cl-project-sample)
             "Can load the new project")
         (finalize))
    (delete-directory-and-files *sample-project-directory*)))

(let ((*sample-project-directory*
       (asdf:system-relative-pathname
        :cl-project
        #p"t/cl-project-sample-eos/")))
  (unwind-protect
       (progn
         (plan 2)
         (cl-project:make-project *sample-project-directory*
                                  :processor 'default-iprocessor)
         (ok (directory-exists-p *sample-project-directory*)
             "Sample project was generated")
         (ok (asdf:load-system :cl-project-sample)
             "Can load the new project")
         (finalize))
    (delete-directory-and-files *sample-project-directory*)))



