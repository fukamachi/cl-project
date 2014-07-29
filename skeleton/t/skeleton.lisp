(in-package :cl-user)
(defpackage <% @var name %>-test
  (:use :cl
        :<% @var name %>
        :cl-test-more))
(in-package :<% @var name %>-test)

;; NOTE: To run this test file, execute `(asdf:test-system :<% @var name %>)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)
