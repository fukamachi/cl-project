#|
  This file is a part of <% @var name %> project.
<% @if author %>  Copyright (c) <%= (local-time:timestamp-year (local-time:now)) %> <% @var author %><% @if email %> (<% @var email %>)<% @endif %>
<% @endif %>|#

(in-package :cl-user)
(defpackage :<% @var test-name %>
  (:use :cl
        :<% @var name %>
        :<% @var test-suite %>))
(in-package :<% @var test-name %>)

<%= (cl-emb:execute-emb
     (merge-pathnames (getf env :test-template) template-path-default)
      :env env :generator-maker generator-maker)
%>

