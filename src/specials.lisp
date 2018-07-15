(in-package :cl-user)
(defpackage cl-project.specials
  (:use #:cl)
  (:export #:*default-skeleton-directory*
           #:*skeleton-directory*
           #:*application-files*
           #:*skeleton-parameters*))
(in-package :cl-project.specials)

(defparameter *default-skeleton-directory*
  (asdf:system-relative-pathname :cl-project #P"skeleton/"))

(defvar *skeleton-directory*
  *default-skeleton-directory*)

(defparameter *default-application-files*
  '("autogen.sh"
    "configure.ac"
    "Makefile.in"
    "compile.lisp"))

(defvar *application-files*
  *default-application-files*)

(defvar *skeleton-parameters* nil)
