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
  :depends-on '(:clack :cl-annot))
;-> writing /Users/fukamachi/Programs/lib/cl-sample/.gitignore
;   writing /Users/fukamachi/Programs/lib/cl-sample/README.markdown
;   writing /Users/fukamachi/Programs/lib/cl-sample/cl-sample-test.asd
;   writing /Users/fukamachi/Programs/lib/cl-sample/cl-sample.asd
;   writing /Users/fukamachi/Programs/lib/cl-sample/src/hogehoge.lisp
;   writing /Users/fukamachi/Programs/lib/cl-sample/t/hogehoge.lisp
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
* `:depends-on`: A list of dependencies.
* `:without-tests`: If true, then no testing system, folder, and file are generated. Default: nil.
* `:verbose`: If true, the written files directories are printed to the standard output. Default: t.

## See Also
- [Rove](https://github.com/fukamachi/rove) - Testing framework

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the MIT License.
