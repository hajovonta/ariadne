;;;; ariadne.asd

(asdf:defsystem #:ariadne
  :description "A graph database in Common Lisp with SPARQL-like query DSL"
  :author "Hajovonta <hajovonta@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:cl-ppcre
               #:bordeaux-threads
               #:hunchentoot
               #:drakma
               #:com.inuoe.jzon
               #:local-time
               #:ironclad
               #:cxml-stp)
  :serial t
  :components ((:file "package")
               (:file "ariadne")
               (:file "prefix")
               (:file "pattern")
               (:file "query")
               (:file "property-graph")
               (:file "traversal")
               (:file "transactions")
               (:file "import-export")
               (:file "persistence")
               (:file "inference")
               (:file "graph-export")
               (:file "repl")
               (:file "streaming")
               (:file "analytics")
               (:file "reactive")
               (:file "sparql-parser")
               (:file "graph-ops")
               (:file "web-server")
               (:file "owl")
               (:file "schema")
               (:file "rdf-xml")
               (:file "profiling")
               (:file "txlog")
               (:file "text-search")
               (:file "events")
               (:file "disk-backed")
               (:file "partitioned")
               (:file "shacl")))
