;;;; package.lisp

(defpackage #:ariadne
  (:use #:cl)
  (:export
   ;; Graph
   #:make-graph
   #:graphp
   #:graph-name
   #:triple-count
   #:clear-graph
   ;; Triples
   #:add-triple
   #:remove-triple
   #:remove-triples
   #:get-triples
   #:has-triple-p
   #:triplep
   #:triple-subject
   #:triple-predicate
   #:triple-object
   ;; Enumeration
   #:all-subjects
   #:all-predicates
   #:all-objects
   ;; Pattern matching
   #:variable-p
   #:lookup-binding
   #:unify
   #:match-pattern
   #:match-patterns
   ;; Query DSL
   #:query
   ;; Property graph
   #:add-node
   #:get-node
   #:remove-node
   #:node-id
   #:node-property
   #:set-node-property
   #:remove-node-property
   #:node-properties
   #:node-labels
   #:find-nodes
   #:add-edge
   #:remove-edge
   #:get-edges
   #:edge-from
   #:edge-to
   #:edge-type
   #:edge-property
   #:neighbors
   ;; Traversal
   #:traverse
   #:traverse-with-path
   #:traverse-depth
   #:shortest-path
   ;; Transactions
   #:with-transaction
   #:begin-transaction
   #:rollback-transaction
   #:transaction-snapshot
   ;; Import/Export
   #:import-ntriples
   #:import-ntriples-file
   #:export-ntriples
   #:import-turtle
   #:export-turtle
   #:import-nquads
   ;; Persistence
   #:save-graph
   #:load-graph
   ;; Inference
   #:defrule
   #:remove-rule
   #:apply-rules
   #:graph-rules
   ;; Export
   #:export-dot
   ;; REPL formatting
   #:format-results
   ;; Streaming import
   #:stream-import-ntriples
   #:stream-import-nquads
   ;; Graph analytics
   #:degree-centrality
   #:connected-components
   #:pagerank
   #:clustering-coefficient
   ;; Reactive
   #:on-match
   #:remove-trigger
   #:graph-triggers))
