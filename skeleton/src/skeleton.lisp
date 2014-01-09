#|
  This file is a part of <% @var name %> project.
<%- @if author %>
  Copyright (c) <%= (local-time:timestamp-year (local-time:now)) %> <% @var author %><% @if email %> (<% @var email %>)<% @endif %>
<%- @endif %>
|#

(in-package :cl-user)
(defpackage <% @var name %>
  (:use :cl))
(in-package :<% @var name %>)

;; blah blah blah.
