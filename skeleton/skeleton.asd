(defsystem "<% @var name %>"
  :version "0.1.0"
  :author "<% @var author %>"
  :license "<% @var license %>"
  :depends-on (<% (format t "~{\"~(~A~)\"~^~%               ~}"
                          (getf env :depends-on)) %>)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "<% @var description %>"
  <%- @unless without-tests %>
  :in-order-to ((test-op (test-op "<% @var name %>/tests")))
  <%- @endunless %>)

