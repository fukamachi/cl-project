#|
  This file is a part of <% @var name %> project.
<%- @if author %>
  Copyright (c) <%= (local-time:timestamp-year (local-time:now)) %> <% @var author %><% @if email %> (<% @var email %>)<% @endif %>
<%- @endif %>
|#

(in-package :cl-user)
(defpackage <% @var name %>-test-asd
  (:use :cl :asdf))
(in-package :<% @var name %>-test-asd)

(defsystem <% @var name %>-test
  :author "<% @var author %>"
  :license "<% @var license %>"
  :depends-on (:<% @var name %>
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "<% @var name %>"))))
  :description "Test system for <% @var name %>"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)))
