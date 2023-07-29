(defsystem "<% @var name %>"
  <%- @if long-name %>
  :long-name "<% @var long-name %>"
  <%- @endif %>
  <%- @if version %>
  :version "<% @var version %>"
  <%- @else %>
  :version "0.0.1"
  <%- @endif %>
  :author "<% @var author %>"
  <%- @if maintainer %>
  :maintainer "<% @var maintainer %>"
  <%- @endif %>
  <%- @if email %>
  :mailto "<% @var email %>"
  <%- @endif %>
  :license "<% @var license %>"
  <%- @if homepage %>
  :homepage "<% @var homepage %>"
  <%- @endif %>
  <%- @if bug-tracker %>
  :bug-tracker "<% @var bug-tracker %>"
  <%- @endif %>
  <%- @if source-control %>
  :source-control "<% @var source-control %>"
  <%- @endif %>
  :depends-on (<% (format t "~{\"~(~A~)\"~^~%               ~}"
                          (getf env :depends-on)) %>)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "<% @var description %>"
  <%- @if long-description %>
  :long-description "<% @var long-description %>"
  <%- @endif %>
  <%- @unless without-tests %>
  :in-order-to ((test-op (test-op "<% @var name %>/tests")))
  <%- @endunless %>)

<%- @unless without-tests %>
(defsystem "<% @var name %>/tests"
  :author "<% @var author %>"
  :license "<% @var license %>"
  :depends-on ("<% @var name %>"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for <% @var name %>"
  :perform (test-op (op c) (symbol-call :rove :run c)))
<%- @endunless %>
