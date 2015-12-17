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

(defun default-skeleton-p (skeleton)
  (equal (skeleton-path skeleton)
         *default-skeleton-directory*))

(defmethod generate ((skeleton skeleton) target-dir)
  (mapcan (lambda (child)
            (unless (and (getf *skeleton-parameters* :without-tests)
                         (default-skeleton-p skeleton)
                         (or
                          ;; Skip test ASD file
                          (string= (file-namestring (template-file-path child))
                                   "skeleton-test.asd")
                          ;; Skip test files
                          (ppcre:scan "t/skeleton\\.lisp$"
                                      (namestring (template-file-path child)))))
              (list (generate child target-dir))))
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
     directory
     (maptree (lambda (file)
                (make-template-file (relative-path file)))
              directory))))
