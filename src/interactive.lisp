#|
This file is a part of cl-project.interactive project.
Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-project.interactive
  (:use :cl :cl-project :iterate :annot.doc)
  (:import-from :osicat
		:environment-variable
		:current-directory))
(in-package :cl-project.interactive)

(cl-syntax:use-syntax :annot)
;; blah blah blah.

(defun yes-no-repl (question)
  (format *query-io* question)
  (read *query-io*))

(defun small-repl (message default)
  (iter
    (with result = default)
    (restart-case
	(error "~a~%input value is : ~s <-- its type is ~s"
	       message result (class-name (class-of result)))
      (change-value ()
	:report (lambda (s)
		  (princ "change and enter a new value." s))
	(format t "~s~% -> :" result)
	(setf result (read))
	(next-iteration))
      (clear-value ()
	:report (lambda (s)
		  (princ "set the value to nil." s))
	(format t "~s~% -> nil~%" result)
	(setf result nil)
	(next-iteration))

      (push-value ()
	:report (lambda (s)
		  (princ "push a new value into the current list." s))
	(cond
	  ((null result)
	   (format t "~a~%pushing : " result)
	   (setf result (list (read))))
	  ((consp result)
	   (format t "~a~%pushing : " result)
	   (push (read) result))
	  (t 
	   (format t "result value is not a list~%")))
	(next-iteration))

      (use-value ()
	:report (lambda (s)
		  (princ "use the current value as the final value." s))
	(return result)))))

@export
@doc "Default dependencies to be inserted in the :DEPENDENCIES
section of the new asdf system definition. You can modify this variable
in your Lisp initialization scripts (such as .sbclrc)."
(defvar *default-dependencies*
  '(:iterate
    :alexandria
    :cl-annot
    :anaphora))

@export
@doc "Creates a new skelton interactively. It also tries to 
create a new repository in the skelton with `git init' command.
In order to change the default dependencies, @see *DEFAULT-DEPENDENCIES* ."
(defun make-project-interactively ()
  (let* ((infos
	  (reduce
	   #'append
	   (mapcar
	    (lambda (lst)
	      (destructuring-bind (key message default type) lst
		(list key
		      (small-repl
		       (format nil "Input ~a. Expected type is ~s."
			       message type)
		       default))))
	    `((:name "the name of your new project/system" "" string)
	      (:description "the short description for the system" "" string)
	      (:author
	       "your name.~% (extracted from your unix user name)"
	       ,(environment-variable :user)
	       string)
	      (:email "your mail address" "" string)
	      (:license "the licence for the system" "LLGPL" string)
	      (:depends-on "the list of package designators" 
			   ,*default-dependencies*
			   (list keyword))))))
	 (path (small-repl
		(format nil "Specify the path for your new project.~%")
		(merge-pathnames
		 (current-directory)
		 (second infos))))
	 (git `(:git ,(yes-no-repl 
		       (format nil 
			       "Do you want to create~
                               a git repo in the specified folder?~%~
                               Answer with nil or t or a string ~
                               specifing the URI of a remote repository.~%")))))
    (apply #'make-project  (list* path (append infos git)))))
