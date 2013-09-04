
(in-package :cl-user)
(defpackage cl-project.git
  (:use :cl :annot.doc)
  (:import-from :inferior-shell :run))
(in-package :cl-project.git)

(cl-syntax:use-syntax :annot)

@export
@doc "create git repo in a path. add the remote origin if possible."
(defun make-git-repo (path origin-path)
  (ensure-directories-exist path)
  (run (format nil "git init ~a" path)
	   :output *standard-output*)
  (when (stringp origin-path)
	(run (format nil "cd ~a;git remote add origin ~a" path origin-path)
		 :output *standard-output*)))