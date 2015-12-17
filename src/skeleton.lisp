(in-package :cl-user)
(defpackage cl-project.skeleton
  (:use #:cl)
  (:import-from #:cl-project.file
                #:make-template-file
                #:generate)
  (:export #:skeleton
           #:make-skeleton
           #:make-skeleton-from-directory))
(in-package :cl-project.skeleton)

(defclass skeleton ()
  ((children :type list
             :initarg :children
             :initform '()
             :accessor skeleton-children)))

(defun make-skeleton (children)
  (make-instance 'skeleton :children children))

(defmethod generate ((skeleton skeleton) target-dir)
  (mapcar (lambda (child)
            (generate child target-dir))
          (skeleton-children skeleton)))

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
     (maptree (lambda (file)
                (make-template-file (relative-path file)))
              directory))))
