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
