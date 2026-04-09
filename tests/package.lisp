;;;; tests/package.lisp
;;;; Test package definition for Ariadne graph database

(defpackage #:ariadne/tests
  (:use #:cl #:ariadne)
  (:local-nicknames (#:bt #:bordeaux-threads))
  (:shadowing-import-from #:ariadne
                          #:export-dot
                          #:defrule
                          #:remove-rule
                          #:apply-rules
                          #:graph-rules)
  (:import-from #:fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is
                #:is-true
                #:is-false
                #:signals
                #:finishes
                #:run
                #:run!
                #:explain!)
  (:export #:run-all-tests
           #:run-suite))

(in-package #:ariadne/tests)

;; =============================================================================
;; Main test suite
;; =============================================================================

(def-suite :ariadne
  :description "Main test suite for Ariadne graph database")

;; =============================================================================
;; Core Triple Store
;; =============================================================================

(def-suite :triple-store
  :description "Core triple store: add, remove, lookup triples"
  :in :ariadne)

;; =============================================================================
;; Indexing
;; =============================================================================

(def-suite :indexing
  :description "SPO, POS, OSP index operations and lookups"
  :in :ariadne)

;; =============================================================================
;; Property Graph Layer
;; =============================================================================

(def-suite :property-graph
  :description "Property graph model: nodes, edges, properties"
  :in :ariadne)

;; =============================================================================
;; Query DSL
;; =============================================================================

(def-suite :query-dsl
  :description "CL query DSL: select, where, filter, optional, union"
  :in :ariadne)

;; =============================================================================
;; SPARQL-like Pattern Matching
;; =============================================================================

(def-suite :sparql-patterns
  :description "SPARQL-like triple pattern matching with logic variables"
  :in :ariadne)

;; =============================================================================
;; Graph Traversal
;; =============================================================================

(def-suite :traversal
  :description "Gremlin-like traversal: out, in, both, path, filter"
  :in :ariadne)

;; =============================================================================
;; Transactions
;; =============================================================================

(def-suite :transactions
  :description "Transaction support: begin, commit, rollback, isolation"
  :in :ariadne)

;; =============================================================================
;; Import/Export (RDF formats)
;; =============================================================================

(def-suite :import-export
  :description "N-Triples, Turtle, and N-Quads import/export"
  :in :ariadne)

;; =============================================================================
;; Persistence
;; =============================================================================

(def-suite :persistence
  :description "Save/load graph to/from disk"
  :in :ariadne)

;; =============================================================================
;; Edge Cases
;; =============================================================================

(def-suite :edge-cases
  :description "Edge cases, empty graphs, duplicate triples, unicode, large graphs"
  :in :ariadne)

;; =============================================================================
;; Advanced Query Features
;; =============================================================================

(def-suite :query-advanced
  :description "GROUP BY, aggregations, ASK, CONSTRUCT, BIND, NOT EXISTS, property paths"
  :in :ariadne)

(def-suite :query-extended
  :description "HAVING, inverse paths, Kleene star, DESCRIBE, REGEX"
  :in :ariadne)

(def-suite :subqueries
  :description "Subqueries and VALUES inline data"
  :in :ariadne)

(def-suite :inference
  :description "Inference rules engine"
  :in :ariadne)

(def-suite :graph-export
  :description "Graph export to DOT/Graphviz"
  :in :ariadne)

(def-suite :turtle-real-world
  :description "Real-world Turtle parsing: 'a' shorthand, comments, booleans, complex patterns"
  :in :ariadne)

(def-suite :w3c-turtle
  :description "W3C Turtle conformance tests"
  :in :ariadne)

(def-suite :repl-formatting
  :description "REPL result formatting as aligned tables"
  :in :ariadne)

(def-suite :turtle-export
  :description "Proper Turtle export with prefixes and shorthand"
  :in :ariadne)

(def-suite :error-handling
  :description "Graceful error handling for malformed input and bad queries"
  :in :ariadne)

(def-suite :streaming-import
  :description "Streaming import for large files"
  :in :ariadne)

(def-suite :examples
  :description "Example datasets and queries"
  :in :ariadne)

(def-suite :graph-analytics
  :description "Graph analytics: PageRank, connected components, degree centrality"
  :in :ariadne)

(def-suite :remaining-sparql
  :description "Sequence paths, GROUP_CONCAT, SAMPLE"
  :in :ariadne)

(def-suite :reactive
  :description "Reactive queries: triggers on pattern match"
  :in :ariadne)

(def-suite :named-graphs
  :description "Named graphs: quads, GRAPH clause"
  :in :ariadne)

(def-suite :sparql-parser
  :description "SPARQL string parser"
  :in :ariadne)

(def-suite :query-planner
  :description "Query planner and optimization"
  :in :ariadne)

(def-suite :compact-index
  :description "Compact index correctness verification"
  :in :ariadne)

(def-suite :thread-safety
  :description "Thread safety: concurrent reads and writes"
  :in :ariadne)

(def-suite :visualization
  :description "Graph visualization via Graphviz"
  :in :ariadne)

(def-suite :graph-operations
  :description "Graph operations: merge, diff, copy, export formats"
  :in :ariadne)

(def-suite :web-server
  :description "Web visualization server"
  :in :ariadne)

(def-suite :owl-reasoning
  :description "OWL/RDFS entailment rules"
  :in :ariadne)

(def-suite :sparql-endpoint
  :description "SPARQL HTTP endpoint"
  :in :ariadne)

;; =============================================================================
;; Helper functions
;; =============================================================================

(defun run-all-tests ()
  "Run all Ariadne tests"
  (run! :ariadne))

(defun run-suite (suite-name)
  "Run a specific test suite"
  (run! suite-name))
