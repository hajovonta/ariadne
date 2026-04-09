;;;; ariadne-tests.asd

(asdf:defsystem #:ariadne-tests
  :description "Test suite for Ariadne graph database"
  :author "Gabor Poczkodi <hajovonta@gmail.com>"
  :license "MIT"
  :depends-on (#:ariadne
               #:fiveam
               #:bordeaux-threads)
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
                             (:file "suite-edge-cases")
                             (:file "suite-query-advanced")
                             (:file "suite-query-extended")
                             (:file "suite-subqueries")
                             (:file "suite-inference")
                             (:file "suite-graph-export")
                             (:file "suite-turtle-real-world")
                             (:file "suite-w3c-turtle")
                             (:file "suite-repl-formatting")
                             (:file "suite-turtle-export")
                             (:file "suite-error-handling")
                             (:file "suite-streaming")
                             (:file "suite-examples")
                             (:file "suite-graph-analytics")
                             (:file "suite-remaining-sparql")
                             (:file "suite-reactive")
                             (:file "suite-named-graphs")
                             (:file "suite-sparql-parser")
                             (:file "suite-query-planner")
                             (:file "suite-compact-index")
                             (:file "suite-thread-safety")
                             (:file "suite-visualization")
                             (:file "suite-graph-operations")
                             (:file "suite-web-server")
                             (:file "suite-owl-reasoning")
                             (:file "suite-sparql-endpoint")
                             (:file "suite-schema-validation")
                             (:file "suite-export-formats"))))
  :perform (asdf:test-op (o c)
                    (uiop:symbol-call :fiveam :run! :ariadne)))
