;;;; ariadne.asd

(asdf:defsystem #:ariadne
  :description "A graph database in Common Lisp with SPARQL-like query DSL"
  :author "Hajovonta <hajovonta@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "ariadne")
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
               (:file "sparql-parser")))
