(in-package :cl-user)
(defpackage cl-project.file
  (:use #:cl
        #:cl-project.specials)
  (:import-from #:cl-project.io
                #:copy-file-to-file)
  (:import-from #:cl-ppcre
                #:regex-replace-all)
  (:export #:template-file
           #:template-file-path
           #:make-template-file
           #:generate))
(in-package :cl-project.file)

(defclass template-file ()
  ((path :type pathname
         :initarg :path
         :accessor template-file-path)))

(defun make-template-file (path)
  (make-instance 'template-file :path path))

(defgeneric generate (file target-dir)
  (:method ((file template-file) target-dir)
    (let ((target-path
            (merge-pathnames (template-file-path file) target-dir)))
      (when (search "skeleton" (pathname-name target-path))
        (setf target-path
              (make-pathname :name
                             (regex-replace-all "skeleton"
                                                (pathname-name target-path)
                                                (getf *skeleton-parameters* :name))
                             :defaults target-path)))
      (copy-file-to-file (merge-pathnames (template-file-path file)
                                          *skeleton-directory*)
                         target-path)
      (list target-path))))
