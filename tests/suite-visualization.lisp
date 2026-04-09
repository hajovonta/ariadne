;;;; tests/suite-visualization.lisp
;;;; Graph visualization via Graphviz

(in-package #:ariadne/tests)
(in-suite :visualization)

(test visualize-graph-png
  "Generate a PNG from a graph"
  (let ((g (make-graph :name "test"))
        (path (merge-pathnames "test-data/test-viz.png"
                               (asdf:system-source-directory :ariadne-tests))))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (visualize-graph g :file path)
    (is-true (probe-file path))
    (when (probe-file path) (delete-file path))))

(test visualize-graph-svg
  "Generate an SVG from a graph"
  (let ((g (make-graph :name "test"))
        (path (merge-pathnames "test-data/test-viz.svg"
                               (asdf:system-source-directory :ariadne-tests))))
    (add-triple g "alice" "knows" "bob")
    (visualize-graph g :file path)
    (is-true (probe-file path))
    (when (probe-file path) (delete-file path))))

(test visualize-graph-engine
  "Use different layout engines"
  (let ((g (make-graph))
        (path (merge-pathnames "test-data/test-neato.png"
                               (asdf:system-source-directory :ariadne-tests))))
    (add-triple g "a" "link" "b")
    (add-triple g "b" "link" "c")
    (add-triple g "c" "link" "a")
    (visualize-graph g :file path :engine :neato)
    (is-true (probe-file path))
    (when (probe-file path) (delete-file path))))

(test visualize-graph-with-filters
  "Visualize with predicate filter and subgraph"
  (let ((g (make-graph))
        (path (merge-pathnames "test-data/test-filtered.png"
                               (asdf:system-source-directory :ariadne-tests))))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "age" 30)
    (add-triple g "bob" "knows" "charlie")
    (visualize-graph g :file path :predicates '("knows") :center "alice" :depth 1)
    (is-true (probe-file path))
    (when (probe-file path) (delete-file path))))

(test describe-graph-summary
  "describe-graph returns a summary string"
  (let ((g (make-graph :name "test")))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "age" 30)
    (add-triple g "bob" "knows" "charlie")
    (let ((summary (describe-graph g)))
      (is (stringp summary))
      (is (search "3 triples" summary))
      (is (search "test" summary)))))
