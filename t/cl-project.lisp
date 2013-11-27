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
        #p"t/sample/")))
  (unwind-protect
       (progn
         (plan 2)
         (when (file-exists-p *sample-project-directory*)
           (delete-directory-and-files *sample-project-directory*))
         (cl-project:make-project *sample-project-directory*)
         (ok (directory-exists-p *sample-project-directory*)
             "Sample project was generated")
         (ok (asdf:load-system :sample)
             "Can load the new project")
         (finalize))
    (delete-directory-and-files *sample-project-directory*)))


(let ((*sample-project-directory*
       (asdf:system-relative-pathname
        :cl-project
        #p"t/sample-no-confirm/")))
  (unwind-protect
       (progn
         (plan 2)
         (when (file-exists-p *sample-project-directory*)
           (delete-directory-and-files *sample-project-directory*))
         (cl-project:make-project *sample-project-directory* :confirm nil)
         (ok (directory-exists-p *sample-project-directory*)
             "Sample project was generated")
         (ok (asdf:load-system :sample-no-confirm)
             "Can load the new project")
         (finalize))
    (delete-directory-and-files *sample-project-directory*)))

(let ((*sample-project-directory*
       (asdf:system-relative-pathname
        :cl-project
        #p"t/sample-mod/")))
  (unwind-protect
       (progn
         (plan 2)
         (cl-project:make-project *sample-project-directory*
                                  :processor 'modified-processor
                                  :author "alien tech")
         (ok (directory-exists-p *sample-project-directory*)
             "Sample project was generated")
         (ok (asdf:load-system :sample-mod)
             "Can load the new project")
         (finalize))
    (delete-directory-and-files *sample-project-directory*)))

(let ((*sample-project-directory*
       (asdf:system-relative-pathname
        :cl-project
        #p"t/sample-eos/")))
  (unwind-protect
       (progn
         (plan 2)
         (cl-project:make-project *sample-project-directory*
                                  :processor 'optima-like-processor)
         (ok (directory-exists-p *sample-project-directory*)
             "Sample project was generated")
         (ok (asdf:load-system :sample-eos)
             "Can load the new project")
         (finalize))
    (delete-directory-and-files *sample-project-directory*)))

;; (let ((*sample-project-directory*
;;        (asdf:system-relative-pathname
;;         :cl-project
;;         #p"t/sample-interactive/")))
;;   (unwind-protect
;;        (progn
;;          (plan 2)
;;          (cl-project:make-project *sample-project-directory*
;;                                   :processor 'default-iprocessor)
;;          (ok (directory-exists-p *sample-project-directory*)
;;              "Sample project was generated")
;;          (ok (asdf:load-system :sample-interactive)
;;              "Can load the new project")
;;          (finalize))
;;     (delete-directory-and-files *sample-project-directory*)))

(let ((*sample-project-directory*
       (asdf:system-relative-pathname
        :cl-project
        #p"t/sample-git/")))
  (unwind-protect
       (progn
         (plan 3)
         (cl-project:make-project *sample-project-directory*
                                  :processor 'git-processor)
         (ok (directory-exists-p *sample-project-directory*)
             "Sample project was generated")
         (let ((git-dir (merge-pathnames ".git" *sample-project-directory*)))
           (ok (directory-exists-p git-dir)
               (format nil "Git repo ~a initialized" git-dir)))
         (ok (asdf:load-system :sample-git)
             "Can load the new project")
         (finalize))
    (delete-directory-and-files *sample-project-directory*)))

