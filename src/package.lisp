(in-package :cl-user)
(defpackage cl-project
  (:use :cl
        :anaphora)
  (:import-from :alexandria
                :once-only)
  (:import-from :cl-fad
                :directory-exists-p
                :pathname-as-directory
                :list-directory)
  (:import-from :trivial-shell
                :shell-command)
  (:import-from :cl-ppcre
                :regex-replace-all)
  (:import-from :cl-emb
                :execute-emb))
