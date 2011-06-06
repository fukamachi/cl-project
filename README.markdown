# CL-Project - Generate a skeleton for modern project

## Usage

    (cl-project:make-project #p"lib/cl-sample
      :author "Eitarow Fukamachi"
      :email "e.arrows@gmail.com"
      :license "LLGPL"
      :depends-on '(clack cl-annot))
    
    ;-> writing /Users/fukamachi/Programs/lib/cl-sample/.gitignore
        writing /Users/fukamachi/Programs/lib/cl-sample/README.markdown
        writing /Users/fukamachi/Programs/lib/cl-sample/cl-sample-test.asd
        writing /Users/fukamachi/Programs/lib/cl-sample/cl-sample.asd
        writing /Users/fukamachi/Programs/lib/cl-sample/src/hogehoge.lisp
        writing /Users/fukamachi/Programs/lib/cl-sample/t/hogehoge.lisp
    ;=> T

## Parameter

All parameters are optional.

* `:name`: Project name. If this key isn't specified, the directory name will be used.
* `:description`: Short description for the new project.
* `:author`: Your name.
* `:email`: Your e-mail address.
* `:license`: License of the new project.
* `:depends-on`: A list of dependency.

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)

## License

Licensed under the LLGPL License.
