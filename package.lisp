;;;; package.lisp

(defpackage #:ariadne
  (:use #:cl)
  (:local-nicknames (#:bt #:bordeaux-threads)
                    (#:ht #:hunchentoot))
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
   #:graph-triggers
   ;; Named graphs
   #:add-quad
   #:get-quads
   #:named-graphs
   ;; SPARQL string parser
   #:sparql
   ;; Visualization
   #:visualize-graph
   #:describe-graph
   ;; Graph operations
   #:merge-graphs
   #:merge-graphs-into
   #:diff-graphs
   #:copy-graph
   #:export-nquads
   ;; Web visualization
   #:start-web-server
   #:stop-web-server
   ;; OWL/RDFS reasoning
   #:apply-owl-rules
   ;; Schema validation
   #:define-schema
   #:graph-schema
   #:validate-graph
   #:find-similar-entities
   ;; Export formats
   #:export-json-ld
   #:export-cytoscape-json
   ;; SPARQL UPDATE
   #:sparql-update
   ;; RDF/XML
   #:import-rdf-xml
   #:import-json-ld
   ;; Profiling
   #:profile-query
   #:graph-statistics
   ;; Transaction log
   #:start-txlog
   #:stop-txlog
   #:replay-txlog
   ;; Pagination
   #:make-query-cursor
   #:cursor-next
   #:cursor-done-p
   ;; Backup
   #:backup-graph
   #:restore-latest-backup
   #:list-backups
   ;; Prefix registry
   #:register-prefix
   #:register-common-prefixes
   #:graph-prefixes
   ;; Memory usage
   #:graph-memory-usage
   ;; Skolemization
   #:skolemize-blank-nodes
   ;; Full-text search
   #:build-text-index
   #:text-search
   #:graph-text-index))
