(in-package :cl-user)
(defpackage cl-project.io
  (:use #:cl
        #:cl-project.specials)
  (:import-from #:cl-emb
                #:execute-emb)
  (:export #:copy-file-to-file))
(in-package :cl-project.io)

(defun copy-file-to-file (source-path target-path)
  "Copy a file `source-path` to the `target-path`."
  (format t "~&writing ~A~%" target-path)
  (ensure-directories-exist target-path)
  (with-open-file (stream target-path :direction :output :if-exists :supersede)
    (write-sequence
     (cl-emb:execute-emb source-path :env *skeleton-parameters*)
     stream)))
