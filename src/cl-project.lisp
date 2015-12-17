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
           #:generate-skeleton))
(in-package :cl-project)

(defun make-project (path &rest params &key name description author email license depends-on &allow-other-keys)
  "Generate a skeleton."
  (declare (ignorable name description author email license depends-on))
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
    (map nil #'load
         (remove-if-not (lambda (path)
                          (string= (pathname-type path) "asd"))
                        files)))
  t)

(defun generate-skeleton (source-dir target-dir &key env)
  "General skeleton generator."
  (let ((*skeleton-parameters* env))
    (generate (make-skeleton-from-directory source-dir) target-dir)))
