(defpackage hoge/tests/main
  (:use :cl
        :hoge
        :rove))
(in-package :hoge/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :hoge)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
