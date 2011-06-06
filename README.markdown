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

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)

## License

Licensed under the LLGPL License.
