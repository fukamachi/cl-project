#|
  This file is a part of CL-Project package.
  URL: http://github.com/fukamachi/cl-project
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  CL-Project is freely distributable under the LLGPL License.
|#

(in-package :cl-project)

(cl-syntax:use-syntax :annot)

@export
(defvar *default-skeleton-directory*
  #.(asdf:system-relative-pathname
       :cl-project
       #p"skeletons/default"))

@export
(defvar *skeleton-directory*
  *default-skeleton-directory*)

@export
(defvar *default-dependency* nil)
@export
(defvar *default-author* nil)
@export
(defvar *default-email* nil)

(defvar *parameters* nil)
(defvar *processor-type*)

@export
(defun make-project (path &rest *parameters* &key
                     (processor 'default-processor)
                     &allow-other-keys)
  "Generate a project from a skeleton.
`path' must be a pathname or a string."

  (let* ((*processor-type* processor)
         (*processor* (make-instance *processor-type*)))

    (loop for (key . value) in (default-values-for *processor*)
       do (unless (member key *parameters*)
            (setf (getf *parameters* key) value)))

    (setf (getf *parameters* :path) path)
    (process-argument-rec *processor* *parameters*)))

(defun process-argument-rec (*processor* list)
  (if list
      (destructuring-bind (key value . rest) list
        (let ((*parameters*
               (process-argument *processor* key value)))
          (process-argument-rec *processor* rest)))
      (let ((asd-files (copy-directory (getf *parameters* :skeleton)
                                       (getf *parameters* :path))))
        (mapc #'load asd-files))))

(defvar *processor*)

@export
(defun generate-skeleton (source-dir target-dir &key *parameters*)
  "General skeleton generator."
  (copy-directory source-dir target-dir))

(defvar *asd-files*)
(defun copy-directory (source-dir target-dir &optional (*asd-files* nil))
  "Copy a directory recursively."
  (ensure-directories-exist target-dir)
  (loop for file in (cl-fad:list-directory source-dir)
     if (cl-fad:directory-pathname-p file)
     do
       (let ((dir (car (last (pathname-directory file)))))
         (unless (string= "includes" dir)
           (cl-emb:register-emb dir dir)
           (copy-directory
            file
            (concatenate 'string
                         (awhen (pathname-device target-dir)
                           (format nil "~A:" it))
                         (directory-namestring target-dir)
                         (cl-emb:execute-emb dir :env *parameters*)
                         "/")
            *asd-files*)))
     else
     do (copy-file-to-dir file target-dir))
  *asd-files*)

(defun copy-file-to-dir (source-path target-dir)
  "Copy a file to target directory."
  (let ((src (pathname-name source-path)))
    (cl-emb:register-emb src src)
    (let ((new-name (cl-emb:execute-emb src :env *parameters*)))
      ;(break "~a" new-name)
      (let ((target-path (make-pathname
                          :device (pathname-device target-dir)
                          :directory (pathname-directory target-dir)
                          :name new-name
                          :type (pathname-type source-path))))
        (copy-file-to-file source-path target-path)
        (when (string= "asd" (pathname-type target-path))
          (push target-path *asd-files*))))))

(defun copy-file-to-file (source-path target-path)
  "Copy a file `source-path` to the `target-path`."
  (format t "~&writing ~A~%" target-path)
  (with-open-file (stream target-path :direction :output :if-exists :supersede)
    (write-sequence
     (cl-emb:execute-emb source-path :env *parameters*)
     stream)))
