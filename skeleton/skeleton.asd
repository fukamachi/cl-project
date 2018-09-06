#|
  This file is a part of <% @var name %> project.
<%- @if author %>
  Copyright (c) <%= (local-time:timestamp-year (local-time:now)) %> <% @var author %><% @if email %> (<% @var email %>)<% @endif %>
<%- @endif %>
|#
<%
(when (or (getf env :description)
          (getf env :author))
%>
#|
<%- @if description %>
  <% @var description %>
  <%- @if author %>
<% @endif %>
<%- @endif %>
<%- @if author %>
  Author: <% @var author %><% @if email %> (<% @var email %>)<% @endif %>
<%- @endif %>
|#
<% ) %>
(asdf:defsystem "<% @var name %>"
  :version "0.1.0"
  :author "<% @var author %>"
  :license "<% @var license %>"
  :depends-on (<% (format t "~{\"~(~A~)\"~^~%               ~}"
                          (getf env :depends-on)) %>)
  :components ((:module "src"
                :components
                ((:file "<% @var name %>"))))
  :description "<% @var description %>"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  <%- @unless without-tests %>
  :in-order-to ((test-op (test-op "<% @var name %>-test")))
  <%- @endunless %>)
