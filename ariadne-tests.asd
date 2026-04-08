;;;; ariadne-tests.asd

(asdf:defsystem #:ariadne-tests
  :description "Test suite for Ariadne graph database"
  :author "Gabor Poczkodi <hajovonta@gmail.com>"
  :license "MIT"
  :depends-on (#:ariadne
               #:fiveam)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "suite-triple-store")
                             (:file "suite-indexing")
                             (:file "suite-property-graph")
                             (:file "suite-query-dsl")
                             (:file "suite-sparql-patterns")
                             (:file "suite-traversal")
                             (:file "suite-transactions")
                             (:file "suite-import-export")
                             (:file "suite-persistence")
                             (:file "suite-edge-cases"))))
  :perform (asdf:test-op (o c)
                    (uiop:symbol-call :fiveam :run! :ariadne)))
