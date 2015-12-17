#|
  This file is a part of CL-Project package.
  URL: http://github.com/fukamachi/cl-project
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  CL-Project is freely distributable under the LLGPL License.
|#

#|
  Generate a skeleton for modern project

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-project-asd
  (:use :cl :asdf))
(in-package :cl-project-asd)

(defsystem cl-project
  :version "0.2"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:cl-emb
               :uiop
               :cl-ppcre
               :local-time
               :prove)
  :components ((:module "src"
                :components
                ((:file "cl-project" :depends-on ("specials" "file" "skeleton"))
                 (:file "file" :depends-on ("specials" "io"))
                 (:file "skeleton" :depends-on ("file"))
                 (:file "io" :depends-on ("specials"))
                 (:file "specials"))))
  :description "Generate a skeleton for modern project"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op cl-project-test))))
