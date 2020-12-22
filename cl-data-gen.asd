;;;; cl-data-gen.asd

(asdf:defsystem #:cl-data-gen
  :description "Library to create arbitrary datastructures using generators."
  :author "Markus Klink <markus.klink@inoio.de>"
  :license  "GPL"
  :version "0.0.1"
  :depends-on (:alexandria :fiveam)
  :components ((
                :module "src"
                :serial t
                :components(
                            (:file "package")
                            (:file "cl-data-gen"))))
  :in-order-to ((test-op (test-op "cl-data-gen/tests"))))

(asdf:defsystem #:cl-data-gen/tests
  :depends-on (:cl-data-gen :fiveam)
  :components ((
                :module "tests"
                :serial t
                :components(
                            (:file "package")
                            (:file "cl-data-gen-tests")
                            )))
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam '#:run!
                                      (uiop:find-symbol* '#:all-tests
                                                         :cl-data-gen-tests))))
