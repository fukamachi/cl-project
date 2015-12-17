(in-package :cl-user)
(defpackage cl-project.skeleton
  (:use #:cl
        #:cl-project.specials)
  (:import-from #:cl-project.file
                #:make-template-file
                #:template-file-path
                #:generate)
  (:export #:skeleton
           #:make-skeleton
           #:make-skeleton-from-directory))
(in-package :cl-project.skeleton)

(defclass skeleton ()
  ((path :type pathname
         :initarg :path
         :accessor skeleton-path)
   (children :type list
             :initarg :children
             :initform '()
             :accessor skeleton-children)))

(defun make-skeleton (path children)
  (make-instance 'skeleton :path path :children children))

(defmethod generate ((skeleton skeleton) target-dir)
  (let ((app (lambda (file)
               (generate file target-dir))))
    (when (getf *skeleton-parameters* :without-tests)
      (setf app
            (funcall cl-project.middleware:*without-tests*
                     app)))
    (mapcan app (skeleton-children skeleton))))

(defun maptree (fn path)
  (flet ((directory-files (path)
           ;; Older ASDF/UIOP's uiop:directory-files returns also directories on Linux.
           ;; The bug had been fixed at ASDF 3.1.0.64 (https://bugs.launchpad.net/asdf/+bug/1276748), however, it's safe to filter directories anyway.
           ;; ref. https://github.com/fukamachi/cl-project/pull/17
           (set-difference (uiop:directory-files path)
                           (uiop:subdirectories path)
                           :test #'equal)))
    (if (uiop:file-pathname-p path)
        ;; file
        (list (funcall fn path))
        ;; directory
        (append
          (mapcar fn (directory-files path))
          (mapcan (lambda (subdir)
                    (maptree fn subdir))
                  (uiop:subdirectories path))))))

(defun make-skeleton-from-directory (directory)
  (flet ((relative-path (path)
           (pathname
            (subseq
             (namestring path)
             (length (namestring directory))))))
    (make-skeleton
     directory
     (maptree (lambda (file)
                (make-template-file (relative-path file)))
              directory))))
