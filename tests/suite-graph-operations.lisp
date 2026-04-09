;;;; tests/suite-graph-operations.lisp
;;;; Graph operations: merge, diff, copy, export formats

(in-package #:ariadne/tests)
(in-suite :graph-operations)

;; =============================================================================
;; Merge
;; =============================================================================

(test merge-graphs-basic
  "Merge two graphs"
  (let ((g1 (make-graph))
        (g2 (make-graph)))
    (add-triple g1 "alice" "knows" "bob")
    (add-triple g2 "bob" "knows" "charlie")
    (let ((merged (merge-graphs g1 g2)))
      (is (= 2 (triple-count merged)))
      (is-true (has-triple-p merged "alice" "knows" "bob"))
      (is-true (has-triple-p merged "bob" "knows" "charlie")))))

(test merge-graphs-dedup
  "Merge deduplicates shared triples"
  (let ((g1 (make-graph))
        (g2 (make-graph)))
    (add-triple g1 "alice" "knows" "bob")
    (add-triple g1 "alice" "age" 30)
    (add-triple g2 "alice" "knows" "bob")
    (add-triple g2 "bob" "age" 25)
    (let ((merged (merge-graphs g1 g2)))
      (is (= 3 (triple-count merged))))))

(test merge-graphs-into
  "Merge into an existing graph"
  (let ((g1 (make-graph))
        (g2 (make-graph)))
    (add-triple g1 "alice" "knows" "bob")
    (add-triple g2 "bob" "knows" "charlie")
    (merge-graphs-into g1 g2)
    (is (= 2 (triple-count g1)))))

;; =============================================================================
;; Diff
;; =============================================================================

(test diff-graphs-added
  "Find triples in g2 but not g1"
  (let ((g1 (make-graph))
        (g2 (make-graph)))
    (add-triple g1 "alice" "knows" "bob")
    (add-triple g2 "alice" "knows" "bob")
    (add-triple g2 "bob" "knows" "charlie")
    (let ((added (diff-graphs g2 g1)))
      (is (= 1 (length added)))
      (is (equal "bob" (triple-subject (first added)))))))

(test diff-graphs-empty
  "Diff of identical graphs is empty"
  (let ((g1 (make-graph))
        (g2 (make-graph)))
    (add-triple g1 "alice" "knows" "bob")
    (add-triple g2 "alice" "knows" "bob")
    (is (= 0 (length (diff-graphs g1 g2))))))

;; =============================================================================
;; Copy
;; =============================================================================

(test copy-graph-independent
  "Copied graph is independent of original"
  (let ((g1 (make-graph :name "original")))
    (add-triple g1 "alice" "knows" "bob")
    (let ((g2 (copy-graph g1)))
      (is (= 1 (triple-count g2)))
      (is (string= "original" (graph-name g2)))
      (add-triple g1 "bob" "knows" "charlie")
      (is (= 1 (triple-count g2)))
      (is (= 2 (triple-count g1))))))

;; =============================================================================
;; Export N-Quads
;; =============================================================================

(test export-nquads-basic
  "Export graph with named graphs as N-Quads"
  (let ((g (make-graph)))
    (add-quad g "alice" "knows" "bob" "http://example.org/g1")
    (let ((nq (export-nquads g)))
      (is (stringp nq))
      (is (search "alice" nq))
      (is (search "example.org/g1" nq)))))

(test export-nquads-roundtrip
  "Export then import N-Quads preserves triples and graph names"
  (let ((g1 (make-graph))
        (g2 (make-graph)))
    (add-quad g1 "alice" "knows" "bob" "http://example.org/g1")
    (add-quad g1 "bob" "knows" "charlie" "http://example.org/g2")
    (let ((nq (export-nquads g1)))
      (import-nquads g2 nq))
    (is (= (triple-count g1) (triple-count g2)))
    (is (= (length (named-graphs g1)) (length (named-graphs g2))))))
