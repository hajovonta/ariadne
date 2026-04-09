;;;; tests/suite-named-graphs.lisp
;;;; Named graphs: quads, GRAPH clause, multi-graph queries

(in-package #:ariadne/tests)
(in-suite :named-graphs)

;; =============================================================================
;; Quad Store (subject, predicate, object, graph)
;; =============================================================================

(test add-quad
  "Add a triple to a named graph"
  (let ((g (make-graph)))
    (add-quad g "alice" "knows" "bob" "http://example.org/g1")
    (is (= 1 (triple-count g)))))

(test add-quad-different-graphs
  "Same triple in different graphs is tracked in both"
  (let ((g (make-graph)))
    (add-quad g "alice" "knows" "bob" "http://example.org/g1")
    (add-quad g "alice" "knows" "bob" "http://example.org/g2")
    ;; One triple, but two graph associations
    (is (= 1 (triple-count g)))
    (is (= 2 (length (named-graphs g))))
    (is (= 1 (length (get-quads g :graph "http://example.org/g1"))))
    (is (= 1 (length (get-quads g :graph "http://example.org/g2"))))))

(test get-quads-by-graph
  "Query triples by graph name"
  (let ((g (make-graph)))
    (add-quad g "alice" "knows" "bob" "http://example.org/g1")
    (add-quad g "charlie" "knows" "dave" "http://example.org/g2")
    (add-triple g "eve" "knows" "frank")
    (let ((g1-triples (get-quads g :graph "http://example.org/g1")))
      (is (= 1 (length g1-triples))))))

(test list-named-graphs
  "List all named graphs"
  (let ((g (make-graph)))
    (add-quad g "alice" "knows" "bob" "http://example.org/g1")
    (add-quad g "charlie" "knows" "dave" "http://example.org/g2")
    (let ((graphs (named-graphs g)))
      (is (= 2 (length graphs))))))

;; =============================================================================
;; GRAPH Clause in Queries
;; =============================================================================

(test query-graph-clause
  "Query within a specific named graph"
  (let ((g (make-graph)))
    (add-quad g "alice" "knows" "bob" "http://example.org/g1")
    (add-quad g "charlie" "knows" "dave" "http://example.org/g2")
    (let ((results (query g '(select (?s ?o)
                              (graph "http://example.org/g1")
                              (where (?s "knows" ?o))))))
      (is (= 1 (length results)))
      (is (equal "alice" (first (first results)))))))

(test query-without-graph-searches-all
  "Query without GRAPH clause searches all graphs"
  (let ((g (make-graph)))
    (add-quad g "alice" "knows" "bob" "http://example.org/g1")
    (add-quad g "charlie" "knows" "dave" "http://example.org/g2")
    (let ((results (query g '(select (?s) (where (?s "knows" ?o))))))
      (is (= 2 (length results))))))

;; =============================================================================
;; N-Quads Import with Graph Names
;; =============================================================================

(test import-nquads-preserves-graph
  "N-Quads import preserves graph names"
  (let ((g (make-graph)))
    (import-nquads g "<http://example.org/a> <http://example.org/b> <http://example.org/c> <http://example.org/g1> .")
    (let ((graphs (named-graphs g)))
      (is (= 1 (length graphs)))
      (is (equal "http://example.org/g1" (first graphs))))))
