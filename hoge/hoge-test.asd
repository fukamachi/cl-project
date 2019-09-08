(defsystem "hoge/tests"
  :author ""
  :license ""
  :depends-on ("hoge"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for hoge"
  :perform (test-op (op c) (symbol-call :rove :run c)))
