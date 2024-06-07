(in-package :cl-user)
(defpackage cl-project
  (:use #:cl)
  (:import-from #:cl-project.specials
                #:*skeleton-directory*
                #:*skeleton-parameters*)
  (:import-from #:cl-project.skeleton
                #:make-skeleton-from-directory)
  (:import-from #:cl-project.file
                #:generate)
  (:import-from #:asdf
		#:load-system)
  (:export #:*skeleton-directory*
           #:make-project
           #:generate-skeleton))
(in-package :cl-project)

(defun make-project (path &rest params &key name long-name version description long-description
                                         author maintainer email license homepage bug-tracker
                                         source-control depends-on (use nil use-p) import-from export
					 (code nil code-p) load-system
                                         (without-tests nil) (verbose t) &allow-other-keys)
  "Generate a skeleton."
  (declare (ignore name long-name version description long-description author maintainer
                   email license homepage bug-tracker source-control depends-on without-tests
		   export))
  (check-type path pathname)

  ;; Ensure `path' ends with a slash(/).
  (setf path (uiop:ensure-directory-pathname path))
  (if code-p
      (when (consp code)
	(setf (getf params :code)
	      (let ((*print-case* :downcase))
		(format nil "~{~S~%~}" code))))
      (setf (getf params :code)
	    ";; blah blah blah."))
  (unless use-p
    (setf (getf params :use)
	  (list :cl)))
  (unless (getf params :name)
    (setf (getf params :name)
          (car (last (pathname-directory path)))))
  (when (or use import-from)
    (when use
      (setf (getf params :depends-on)
	    (append (getf params :depends-on) use)))
    (when import-from
      (setf (getf params :depends-on)
	    (append (getf params :depends-on)
		    (remove nil
			    (mapcar (lambda (dependency)
				      (cond ((consp dependency)
					     (first dependency))
					    ((atom dependency)
					     dependency)))
				    import-from)))))
    (setf (getf params :depends-on)
	  (remove-duplicates
	   (remove "CL" (getf params :depends-on)
		   :test #'string-equal)
	   :test #'string-equal)))
  (let ((files (generate-skeleton
                *skeleton-directory*
                path
                :env params
                :verbose verbose)))
    (dolist (file files)
      (when (string= (pathname-type file) "asd")
        (let ((dir (make-pathname :name nil :type nil :defaults file)))
          (push dir asdf:*central-registry*)))))
  (when load-system
    (load-system (getf params :name)))
  t)

(defun generate-skeleton (source-dir target-dir &key env verbose)
  "General skeleton generator."
  (let ((*skeleton-parameters* env))
    (generate (make-skeleton-from-directory source-dir) target-dir
              :verbose verbose)))
