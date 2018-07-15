(in-package #:cl-user)

(load "<% @var name %>.asd")
(asdf:oos 'asdf:load-op '<% @var name %>)

#+sbcl
(sb-ext:save-lisp-and-die "<% @var name %>"
                          :compression t
                          :toplevel (lambda () 
                                      (<% @var name %>:main sb-ext:*posix-argv*)
                                      0)
                          :executable t)

#+ecl
(asdf:make-build '<% @var name %> :type :program :monolithic t
                 :move-here "."
                 :name-suffix ""
                 :epilogue-code '(progn
                                  (<% @var name %>:main (ext:command-args))))

#+ccl
(ccl:save-application "<% @var name %>"
                      :prepend-kernel t
                      :toplevel-function #'<% @var name %>:main)
