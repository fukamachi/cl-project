(defpackage <% @var name %>/tests/main
  (:use :cl
        :<% @var name %>
        :rove))
(in-package :<% @var name %>/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :<% @var name %>)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
