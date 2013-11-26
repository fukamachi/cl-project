(in-package :cl-project)
(cl-syntax:use-syntax :annot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; utility macros

(defmacro delay (&body body)
  `(lambda () ,@body))
(defmacro force (thing)
  (once-only (thing)
    `(if (functionp ,thing)
         (funcall ,thing)
         ,thing)))

(defmacro setp (key value)
  `(progn (setf (getf *parameters* ,key) ,value)
          *parameters*))
(defmacro getp (key)
  `(force (getf *parameters* ,key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; generic function

(defclass processor ()
  ())

(defgeneric process-argument (processor key value))
(defmethod process-argument :around ((processor processor) key value)
  (setp key (call-next-method))
  *parameters*)

(defmethod process-argument ((p processor) k (v function))
  (funcall v))
(defmethod process-argument ((p processor) k v)
  v)
(defgeneric default-values-for (processor)
  (:documentation "alist of ((key . value)* )")
  (:method-combination append))

(defmethod default-values-for append ((processor processor))
  `((:skeleton . ,*default-skeleton-directory*)
    (:author . ,(delay (string-right-trim '(#\Space #\Newline) (shell-command "whoami"))))
    (:name . ,(delay (car (last (pathname-directory (getp :path))))))
    (:depends-on . ,*default-dependency*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; non-interactive processor

(defmethod process-argument ((p processor) (k (eql :path)) target)
  ;; Ensure `path' ends with a slash(/).
  (fad:pathname-as-directory target))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; interactive processor


(defclass markdown-readme-mixin () ())
(defmethod default-values-for append ((p markdown-readme-mixin))
  `((:readme . "includes/README.markdown")))

(defclass org-readme-mixin () ())
(defmethod default-values-for append ((p org-readme-mixin))
  `((:readme . "includes/README.org")))



(defclass src-dir-mixin () ())
(defmethod default-values-for append ((p src-dir-mixin))
  `((:source-dir . "src")))

(defclass t-dir-mixin () ())
(defmethod default-values-for append ((p t-dir-mixin))
  `((:test-dir . "t")))

(defclass test-dir-mixin () ())
(defmethod default-values-for append ((p test-dir-mixin))
  `((:test-dir . "test")))



(defclass test-package-hyphened-mixin () ())
(defmethod default-values-for append ((p test-package-hyphened-mixin))
  `((:test-name . ,(delay (concatenate 'string (getp :name) "-test")))))

(defclass test-package-dotted-mixin () ())
(defmethod default-values-for append ((p test-package-dotted-mixin))
  `((:test-name . ,(delay (concatenate 'string (getp :name) ".test")))))

(defclass test-package-dotted-t-mixin () ())
(defmethod default-values-for append ((p test-package-dotted-t-mixin))
  `((:test-name . ,(delay (concatenate 'string (getp :name) ".t")))))



(defclass package-file-as-project-mixin () ())
(defmethod default-values-for append ((p package-file-as-project-mixin))
  `((:package-filename . ,(delay (getp :name)))))

(defclass package-file-as-package-mixin () ())
(defmethod default-values-for append ((p package-file-as-package-mixin))
  `((:package-filename . :package)))


(defclass asdf3-mixin () ())
(defmethod default-values-for append ((p asdf3-mixin))
  `((:asdf3 . t)))


(defclass cl-test-more-mixin () ())
(defmethod default-values-for append ((p cl-test-more-mixin))
  `((:test-suite . "cl-test-more")
    (:test-template . "includes/cl-test-more")
    (:test-command . (asdf:clear-system c))))
(defclass fiveam-mixin () ())
(defmethod default-values-for append ((p fiveam-mixin))
  `((:test-suite . "fiveam")
    (:test-template . "includes/fiveam")
    (:test-command . ,(delay
                       `(progn
                          (eval (read-from-string
                                 ,(format nil "(fiveam:run! :~a)" (getp :name))))
                          (asdf:clear-system c))))))

(defclass default-processor (processor
                             markdown-readme-mixin
                             src-dir-mixin
                             t-dir-mixin
                             test-package-hyphened-mixin
                             cl-test-more-mixin
                             package-file-as-project-mixin)
  ())

;; (defclass interactive-processor (processor)
;;   ())

;; (defmethod process-argument ((p interactive-processor) (k (eql :name)) v)
;;   (if (or (and (query-overwrite :name *parameters*)
;;       (call-next-method)
;;       (set-param :name (query-string))))

;; (defmethod process-argument ((p interactive-processor) (k (eql :author)) v)
;;   )

;; (defmethod process-argument ((p interactive-processor) (k (eql :description)) v)
;;   )

;; (defmethod process-argument ((p interactive-processor) (k (eql :email)) v)
;;   )

;; (defmethod process-argument ((p interactive-processor) (k (eql :licence)) v)
;;   )

;; (defmethod process-argument ((p interactive-processor) (k (eql :depends-on)) v)
;;   )


;; (defclass git-user-mixin ()
;;   ())

;; git config --global --get user.email
