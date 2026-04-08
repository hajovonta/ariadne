;;;; tests/package.lisp
;;;; Test package definition for Ariadne graph database

(defpackage #:ariadne/tests
  (:use #:cl #:ariadne)
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
;; Helper functions
;; =============================================================================

(defun run-all-tests ()
  "Run all Ariadne tests"
  (run! :ariadne))

(defun run-suite (suite-name)
  "Run a specific test suite"
  (run! suite-name))
