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
  (:export #:*skeleton-directory*
           #:make-project
           #:generate-skeleton
           #:add-to-init-file))
(in-package :cl-project)

(defun get-init-file-name ()
  "Determines the init file name for different CL implementations."
  (or
   #+sbcl
   ".sbclrc"
   #+allegro
   ".clinit.cl"
   #+abcl
   ".abclrc"
   #+(and ccl windows)
   "ccl-init.lisp"
   #+(and ccl (not windows))
   ".ccl-init.lisp"
   #+clasp
   ".clasprc"
   #+clisp
   ".clisprc.lisp"
   #+cmu
   ".cmucl-init.lisp"
   #+scl
   ".scl-init.lisp"
   #+lispworks
   ".lispworks"
   #+ecl
   ".eclrc"
   #+mkcl
   ".mkclrc" nil))

(defun add-to-init-file (path &rest params &key name)
  "Adds the application to the CL implementation's init file to load generated application at every startup of CL."
  
  ;; Ensure `path' ends with a slash(/).
  (setf path (uiop:ensure-directory-pathname path))
  
  (unless (getf params :name)
    (setf (getf params :name)
          (car (last (pathname-directory path)))))
  
  (let ((init-file-name (get-init-file-name)))
    (when init-file-name
      (let ((init-file-path (merge-pathnames init-file-name (user-homedir-pathname))))
	(with-open-file (stream init-file-path 
				:direction :output 
				:if-exists :append 
				:if-does-not-exist :create)
	  (format stream 
		  "~%~%~%~A~A~%~A~S~A~%~%~%"
		  "#-" 
		  (getf params :name)
		  "(pushnew " path " asdf:*central-registry*)"))))))

(defun make-project (path &rest params &key name description author email license depends-on (without-tests nil) load-at-startup &allow-other-keys)
  "Generate a skeleton."
  (declare (ignore name description author email license depends-on without-tests load-at-startup))
  (check-type path pathname)

  ;; Ensure `path' ends with a slash(/).
  (setf path (uiop:ensure-directory-pathname path))

  (unless (getf params :name)
    (setf (getf params :name)
          (car (last (pathname-directory path)))))
  (let ((files (generate-skeleton
                *skeleton-directory*
                path
                :env params)))
    (dolist (file files)
      (when (string= (pathname-type file) "asd")
        (let ((dir (make-pathname :name nil :type nil :defaults file)))
          (push dir asdf:*central-registry*)))))
  (when (getf params :load-at-startup)
    (add-to-init-file path :name (getf params :name)))
  t)

(defun generate-skeleton (source-dir target-dir &key env)
  "General skeleton generator."
  (let ((*skeleton-parameters* env))
    (generate (make-skeleton-from-directory source-dir) target-dir)))
