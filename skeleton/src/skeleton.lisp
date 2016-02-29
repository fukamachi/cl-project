(in-package :cl-user)
(defpackage <% @var name %>
            (:use :cl)<% @if application %>(:export :main)<% @endif %>)
(in-package :<% @var name %>)


<% @if application %>
(defun main (args)
  ;; Here will be your application
  0 ;; return code
  )
<% @else %>
;; Here will be your applicaton/library
<% @endif %>
