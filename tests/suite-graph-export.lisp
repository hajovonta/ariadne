;;;; tests/suite-graph-export.lisp
;;;; Graph export to DOT/Graphviz format

(in-package #:ariadne/tests)
(in-suite :graph-export)

;; =============================================================================
;; DOT Export
;; =============================================================================

(test export-dot-basic
  "Export graph as DOT format"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (let ((dot (export-dot g)))
      (is (stringp dot))
      (is (search "digraph" dot))
      (is (search "alice" dot))
      (is (search "bob" dot))
      (is (search "knows" dot)))))

(test export-dot-with-name
  "DOT export uses graph name"
  (let ((g (make-graph :name "social")))
    (add-triple g "alice" "knows" "bob")
    (let ((dot (export-dot g)))
      (is (search "social" dot)))))

(test export-dot-empty-graph
  "DOT export of empty graph"
  (let ((g (make-graph :name "empty")))
    (let ((dot (export-dot g)))
      (is (search "digraph" dot))
      (is (search "empty" dot)))))

(test export-dot-filters-predicates
  "DOT export can filter by predicate"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "age" 30)
    (let ((dot (export-dot g :predicates '("knows"))))
      (is (search "knows" dot))
      (is-false (search "age" dot)))))

(test export-dot-subgraph
  "DOT export of a subgraph around a node"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (add-triple g "charlie" "knows" "dave")
    (let ((dot (export-dot g :center "bob" :depth 1)))
      (is (search "alice" dot))
      (is (search "bob" dot))
      (is (search "charlie" dot))
      ;; dave is 2 hops from bob, should not appear
      (is-false (search "dave" dot)))))

(test export-dot-to-file
  "DOT export to file"
  (let ((g (make-graph))
        (path (merge-pathnames "test-data/test.dot"
                               (asdf:system-source-directory :ariadne-tests))))
    (add-triple g "alice" "knows" "bob")
    (export-dot g :file path)
    (is-true (probe-file path))
    (let ((content (uiop:read-file-string path)))
      (is (search "digraph" content)))
    (when (probe-file path) (delete-file path))))
