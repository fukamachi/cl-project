#|
  This file is a part of CL-Project package.
  URL: http://github.com/fukamachi/cl-project
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  CL-Project is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage cl-project
  (:use :cl
	:annot.doc)
  (:import-from :osicat
                :directory-exists-p
		:directory-pathname-p
                :pathname-as-directory
                :list-directory)
  (:import-from :cl-ppcre
                :regex-replace-all)
  (:import-from :inferior-shell
                :run)
  (:import-from :cl-emb
                :execute-emb)
  (:import-from :cl-project.git
		:make-git-repo))
(in-package :cl-project)

(cl-syntax:use-syntax :annot)

@export
@doc "A pathname to the official skeleton."
(defvar *official-skeleton*
  #.(asdf:system-relative-pathname
     :cl-project
     #p"skeleton/"))

@export
@doc "Specifies the directory of a user-defined skelton. Defaulted to
the `skeleton/' directory in the :CL-PROJECT system directory."
(defvar *skeleton-directory*
  *official-skeleton*)

(defvar *skeleton-parameters* nil)

(defun ->string (desig)
  (etypecase desig
    (string desig)
    (symbol (symbol-name desig))))

@export
(defun make-project (path &rest params
		     &key name description author
		     email license depends-on git &allow-other-keys)
  "Generate a skeleton.
`path' : a PATHNAME or a STRING.
`name', `license' : a STRING-DESIGNATOR.
`description', `author', `email' : a STRING.
`git' : a BOOLEAN or a STRING. when non-nil, creates a git repository in the folder. If it is a STRING indicating a git URL, it adds that repo as the origin."  
  (declare (ignorable name description author email license depends-on))

  ;; Ensure `path' ends with a slash(/).
  (setf path (pathname-as-directory path))

  ;; ensure that PARAMS have the correct values
  (let ((str (if name
		 (string-downcase (->string name))
		 (car (last (pathname-directory path))))))
    (setf (getf params :name) str name str))
  
  (generate-skeleton
   *skeleton-directory*
   path
   :env params)
  (when git
    (make-git-repo path git))
  (load (merge-pathnames (concatenate 'string name ".asd")
                         path))
  (load (merge-pathnames (concatenate 'string name "-test.asd")
                         path)))

@export
(defun generate-skeleton (source-dir target-dir &key env)
  "General skeleton generator."
  (let ((*skeleton-parameters* env))
    (copy-directory source-dir target-dir)))

(defun copy-directory (source-dir target-dir)
  "Copy a directory recursively."
  (ensure-directories-exist target-dir :verbose t)
  (loop for file in (list-directory source-dir)
     if (directory-pathname-p file)
     do (copy-directory
	 file
	 (concatenate 'string
		      (let ((device (pathname-device target-dir)))
			(when device
			  (format nil "~A:" device)))
		      (directory-namestring target-dir)
		      (car (last (pathname-directory file))) "/"))
     else
     do (copy-file-to-dir file target-dir))
  t)

(defun make-project-target-pathname (source-path target-dir name)
  (make-pathname
   :device (pathname-device target-dir)
   :directory (pathname-directory target-dir)
   :name (regex-replace-all
	  "skeleton"
	  (pathname-name source-path)
	  (string-downcase name))
   :type (pathname-type source-path)))

(defun copy-file-to-dir (source-path target-dir)
  "Copy a file to target directory."
  (copy-file-to-file
   source-path
   (make-project-target-pathname
    source-path target-dir (getf *skeleton-parameters* :name))))

(defun copy-file-to-file (source-path target-path)
  "Copy a file `source-path` to the `target-path`."
  (format t "~&writing ~A~%" target-path)
  (with-open-file (stream target-path
			  :direction :output
			  :if-exists :supersede)
    (write-sequence
     (cl-emb:execute-emb source-path :env *skeleton-parameters*)
     stream)))

@export
@doc "Copies the official skeleton to the specified directory
and interectively adds some scripts to the initialization file such as \".sbclrc\".
The scripts, shortly, do (setf dir *skeleton-directory*) .

Useful when you want to create a user-defined skeleton.

Usage: (make-skeleton #p\"path/to/your/skeleton/\")
"
(defun make-skeleton (path)
  (setf path (pathname-as-directory path))
  (ensure-directories-exist path :verbose t)
  (run `(cp -r ,*official-skeleton* ,path) :show t)
  (let* ((skeleton (merge-pathnames "skeleton" path))
	 (init-file-name
	  #+allegro ".clinit.cl"
	  #+abcl ".abclrc"
	  #+(and ccl windows) "ccl-init.lisp"
	  #+(and ccl (not windows)) ".ccl-init.lisp"
	  #+clisp ".clisprc.lisp"
	  #+ecl		    ".eclrc"
	  #+mkcl	    ".mkclrc"
	  #+sbcl	    ".sbclrc"
	  #+lispworks	    ".lispworks"
	  #+cmucl	    ".cmucl-init.lisp"
	  #+scl	    ".scl-init.lisp"
	  #-(or cmucl scl sbcl allegro clisp lispworks ecl abcl ccl mkcl) nil)
	 (form `(progn
		  (ql:quickload :cl-project)
		  (setf (symbol-value
			 (find-symbol "*SKELETON-DIRECTORY*"
				      (FIND-PACKAGE :cl-project)))
			,skeleton))))
    (format t "Created a skeleton directory at ~w ." 
	    skeleton)
    (if init-file-name
	(let ((init (merge-pathnames
		     init-file-name
		     (user-homedir-pathname)))
	      (old (merge-pathnames
		    (format nil "~a.old" init-file-name)
		    (user-homedir-pathname))))
	  (when (yes-or-no-p "
In order to use this skeleton by default,
add to the init file ~w the following:

~w

Be sure that the init file is already configured
so that it loads quicklisp. (asdf-install is obsoleted.)
If you don't have quicklisp installed, go http://www.quicklisp.org/beta/ .

I can add the above code automatically.
The old init file will be renamed as ~w .
Would you mind if I do it for you?"
			     init
			     form
			     old)
	    (run `(cp ,init ,old) :show t)
	    (with-open-file (s init
			       :direction :output
			       :if-does-not-exist :create
			       :if-exists :append)
	      (terpri s)
	      (write form :stream s))))
	
	(format t "
In order to use this skeleton by default,
add to the init file (failed to auto-detect) the following:

~w

Be sure that the init file is already configured
so that it loads quicklisp. (asdf-install is obsoleted.)
If you don't have quicklisp installed, go http://www.quicklisp.org/beta/ ."
		form))))
