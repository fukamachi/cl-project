# CL-Project - Generate modern project skeletons

[![Build Status](https://travis-ci.org/fukamachi/cl-project.svg?branch=master)](https://travis-ci.org/fukamachi/cl-project)
[![Quicklisp dist](http://quickdocs.org/badge/cl-project.svg)](http://quickdocs.org/cl-project/)

## Usage

```common-lisp
(cl-project:make-project #p"lib/cl-sample/"
  :name "cl-sample"
  :long-name "common-lisp-sample"
  :author "Eitaro Fukamachi"
  :maintainer "Eitaro Fukamachi"
  :email "e.arrows@gmail.com"
  :license "MIT"
  :homepage "https://github.com/fukamachi/cl-project"
  :bug-tracker "https://github.com/fukamachi/cl-project/issues"
  :source-control "https://github.com/fukamachi/cl-project.git"
  :version "0.1.1"
  :description "Sample library"
  :long-description "Common Lisp sample library"
  :depends-on '(:clack alexandria)
  :use '(:cl)
  :import-from '(:clack (serapeum concat))
  :export '(test-function test-constant)
  :code '((alexandria:define-constant test-constant "hallo" :test 'string=)
          (defun test-function (user)
            "docstring"
            (concat test-constant " " user))))
;-> writing /Users/fukamachi/Programs/lib/cl-sample/.gitignore
;   writing /Users/fukamachi/Programs/lib/cl-sample/README.markdown
;   writing /Users/fukamachi/Programs/lib/cl-sample/cl-sample-test.asd
;   writing /Users/fukamachi/Programs/lib/cl-sample/cl-sample.asd
;   writing /Users/fukamachi/Programs/lib/cl-sample/src/main.lisp
;   writing /Users/fukamachi/Programs/lib/cl-sample/t/main.lisp
;=> T
```

## What's the difference from other generators?

### 1. Flexible templates

CL-Project supports more parameters to embed, by using [CL-EMB](http://common-lisp.net/project/cl-emb/) to represent the skeleton files (See "cl-project/skeleton/").

### 2. One package per file style (Modern)

A modern CL project should be in accordance with [some rules](http://labs.ariel-networks.com/cl-style-guide.html). For instance, one file must have one package in it.

### 3. Recommends unit testing

Modern projects should have some unit tests. CL-Project generates a system for unit testing, so you can begin writing unit tests as soon as the project is generated.

## Parameters

All parameters are optional.

* `:name`: Project name. If this key isn't specified, the directory name will be used.
* `:long-name`: Project name long form.
* `:description`: Short description for the new project.
* `:long-description`: Long description for the new project.
* `:version`: Project version.
* `:author`: Your name.
* `:maintainer`: Project maintainer.
* `:email`: Your e-mail address.
* `:license`: License of the new project.
* `:homepage`: Project homepage.
* `:bug-tracker`: Project bug-tracker. E.g. Git issue tracker.
* `:source-control`: Project source-control.
* `:depends-on`: A list of dependencies for the system definition. If `use` or `import-from` is specified, the respective packages are added to this list automatically.
* `:without-tests`: If true, then no testing system, folder, and file are generated. Default: nil.
* `:verbose`: If true, the written files directories are printed to the standard output. Default: t.
* `:use`: A list of packages that will be in the use-clause of the package definition. If you do not want to use any package, then supply `nil`. Default value: `'(:cl)`.
* `:import-from`: A list of packages and symbols that will be in import-from-clauses in the package definition. Value can be a list or nested list.
* `:export`: A list of symbols that will be exported from the package definition.
* `:code`: Source code that will be inserted in main.lisp file. Can be nil, string, or a list of forms. Make sure to use, import-from, or qualify the necessary symbols.

## See Also
- [Rove](https://github.com/fukamachi/rove) - Testing framework

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the MIT License.
