(in-package :cl-user)
(defpackage cl-project.specials
  (:use #:cl)
  (:export #:*default-skeleton-directory*
           #:*skeleton-directory*
           #:*skeleton-parameters*))
(in-package :cl-project.specials)

(defparameter *default-skeleton-directory*
  (asdf:system-relative-pathname :cl-project #P"skeleton/"))

(defvar *skeleton-directory*
  *default-skeleton-directory*)

(defvar *skeleton-parameters* nil)
