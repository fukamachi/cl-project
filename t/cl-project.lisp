#|
  This file is a part of CL-Project project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-project-test
  (:use :cl
        :asdf
        :trivial-shell
        :cl-project
        :cl-test-more)
  (:import-from :cl-fad
                :file-exists-p
                :directory-exists-p
                :delete-directory-and-files))
(in-package :cl-project-test)


(defvar *systems* nil)

(defun sys-push (path)
  (let ((path2 (system-relative-pathname
                :cl-project path)))
    (push path2 *systems*)
    path2))

(let ((dir (sys-push #p"t/sample/")))
  (plan 2)
  (when (file-exists-p dir)
    (delete-directory-and-files dir))
  (cl-project:make-project dir)
  (ok (directory-exists-p dir)
      "Sample project was generated")
  (ok (load-system :sample)
      "Can load the new project"))

;; same, but with no confirmation
(let ((dir (sys-push #p"t/sample-no-confirm/")))
  (plan 2)
  (when (file-exists-p dir)
    (delete-directory-and-files dir))
  (cl-project:make-project dir :confirm nil)
  (ok (directory-exists-p dir)
      "Sample project was generated")
  (ok (load-system :sample-no-confirm)
      "Can load the new project"))

;; modified processor
(let ((dir (sys-push #p"t/sample-mod/")))
  (plan 2)
  (when (file-exists-p dir)
    (delete-directory-and-files dir))
  (cl-project:make-project dir
                           :processor 'modified-processor
                           :author "alien tech")
  (ok (directory-exists-p dir)
      "Sample project was generated")
  (ok (load-system :sample-mod)
      "Can load the new project"))

;; eos processor
(let ((dir (sys-push #p"t/sample-eos/")))
  (plan 2)
  (when (file-exists-p dir)
    (delete-directory-and-files dir))
  (cl-project:make-project dir
                           :processor 'optima-like-processor)
  (ok (directory-exists-p dir)
      "Sample project was generated")
  (ok (load-system :sample-eos)
      "Can load the new project"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; git

(let ((dir (sys-push #p"t/sample-git/")))
  (plan 3)
  (when (file-exists-p dir)
    (delete-directory-and-files dir))
  (cl-project:make-project dir
                           :processor 'git-processor)
  (ok (directory-exists-p dir)
      "Sample project was generated")
  (let ((git-dir (merge-pathnames ".git" dir)))
    (ok (directory-exists-p git-dir)
        (format nil "Git repo ~a initialized" git-dir)))
  (ok (load-system :sample-git)
      "Can load the new project")
  (is (system-author (find-system :sample-git))
      (git-name)
      "Correct author information provided")
  (is (system-mailto (find-system :sample-git))
      (git-email)
      "Correct email information provided"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; interactive

(defclass git-iprocessor (interactive-processor
                          markdown-readme-mixin
                          src-dir-mixin
                          t-dir-mixin
                          test-package-hyphened-mixin
                          cl-test-more-mixin
                          package-file-as-project-mixin
                          git-mixin)
  ())

(let ((dir (sys-push #p"t/sample-interactive/")))
  (plan 2)
  (when (file-exists-p dir)
    (delete-directory-and-files dir))
  (cl-project:make-project dir
                           :processor 'git-iprocessor)
  (ok (directory-exists-p dir)
      "Sample project was generated")
  (ok (load-system :sample-interactive)
      "Can load the new project"))

(defun clean ()
  (dolist (dir (mapcar (lambda (pathname)
                         (system-relative-pathname
                          :cl-project pathname))
                       *systems*))
    (when (file-exists-p dir)
      (delete-directory-and-files dir))))

(finalize)