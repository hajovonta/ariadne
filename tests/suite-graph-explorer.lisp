;;;; tests/suite-graph-explorer.lisp
;;;; Graph explorer Phase 1 tests: Visibility & Navigation

(in-package #:ariadne/tests)

(def-suite :graph-explorer
  :description "Graph explorer visibility and navigation"
  :in :ariadne)

(in-suite :graph-explorer)

;;; --- /api/types endpoint ---

(test types-api-returns-type-map
  "Returns a mapping of rdf:type values to colors"
  (let ((g (make-graph)))
    (add-triple g "http://ex.org/alice" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "http://ex.org/Person")
    (add-triple g "http://ex.org/bob" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "http://ex.org/Person")
    (add-triple g "http://ex.org/acme" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "http://ex.org/Company")
    (let ((result (ariadne::graph-types-json g)))
      (is (stringp result))
      ;; Contains both types
      (is (search "Person" result))
      (is (search "Company" result))
      ;; Contains color assignments (hex colors)
      (is (search "#" result)))))

(test types-api-empty-graph
  "Empty graph returns empty type map"
  (let* ((g (make-graph))
         (result (ariadne::graph-types-json g)))
    (is (stringp result))
    (is (search "{}" result))))

(test types-api-nodes-without-type
  "Nodes without rdf:type are not in the type map"
  (let ((g (make-graph)))
    (add-triple g "http://ex.org/alice" "http://ex.org/knows" "http://ex.org/bob")
    (let ((result (ariadne::graph-types-json g)))
      (is (search "{}" result)))))

;;; --- /api/node endpoint ---

(test node-api-returns-node-details
  "Returns all triples where node is subject or object"
  (let ((g (make-graph)))
    (add-triple g "http://ex.org/alice" "http://ex.org/knows" "http://ex.org/bob")
    (add-triple g "http://ex.org/alice" "http://ex.org/age" "30")
    (add-triple g "http://ex.org/bob" "http://ex.org/knows" "http://ex.org/alice")
    (let ((result (ariadne::node-info-json g "http://ex.org/alice")))
      (is (stringp result))
      ;; Contains outgoing edges
      (is (search "knows" result))
      (is (search "age" result))
      ;; Contains incoming edge from bob
      (is (search "bob" result)))))

(test node-api-nonexistent-node
  "Nonexistent node returns empty result"
  (let* ((g (make-graph))
         (result (ariadne::node-info-json g "http://ex.org/nobody")))
    (is (stringp result))
    (is (search "outgoing" result))
    (is (search "incoming" result))))

;;; --- /api/graph with center+depth ---

(test graph-api-center-depth
  "Center+depth returns correct subgraph"
  (let ((g (make-graph)))
    (add-triple g "a" "knows" "b")
    (add-triple g "b" "knows" "c")
    (add-triple g "c" "knows" "d")
    (let ((json (ariadne::graph-to-cytoscape-json g :center "a" :depth 1)))
      ;; a and b should be present (depth 1 from a)
      (is (search "\"a\"" json))
      (is (search "\"b\"" json))
      ;; d should NOT be present (depth 3 from a)
      (is (not (search "\"d\"" json))))))

(test graph-api-center-depth-2
  "Depth 2 includes two hops"
  (let ((g (make-graph)))
    (add-triple g "a" "knows" "b")
    (add-triple g "b" "knows" "c")
    (add-triple g "c" "knows" "d")
    (let ((json (ariadne::graph-to-cytoscape-json g :center "a" :depth 2)))
      (is (search "\"a\"" json))
      (is (search "\"b\"" json))
      (is (search "\"c\"" json))
      (is (not (search "\"d\"" json))))))

;;; --- Node-to-type color assignment ---

(test type-color-assignment-consistent
  "Same type always gets the same color"
  (let ((g (make-graph)))
    (add-triple g "http://ex.org/alice" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "http://ex.org/Person")
    (let ((r1 (ariadne::graph-types-json g))
          (r2 (ariadne::graph-types-json g)))
      (is (string= r1 r2)))))

(test type-color-assignment-distinct
  "Different types get different colors"
  (let ((g (make-graph)))
    (add-triple g "http://ex.org/alice" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "http://ex.org/Person")
    (add-triple g "http://ex.org/acme" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "http://ex.org/Company")
    (let ((result (ariadne::graph-types-json g)))
      ;; Parse and check colors are different
      (let ((parsed (com.inuoe.jzon:parse result)))
        (let ((colors (loop for v being the hash-values of parsed collect v)))
          (is (= 2 (length (remove-duplicates colors :test #'string=)))))))))

;;; --- Cytoscape elements include type data ---

(test cytoscape-nodes-include-type
  "Cytoscape node data includes type field for coloring"
  (let ((g (make-graph)))
    (add-triple g "http://ex.org/alice" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "http://ex.org/Person")
    (add-triple g "http://ex.org/alice" "http://ex.org/knows" "http://ex.org/bob")
    (let ((json (ariadne::graph-to-cytoscape-json g)))
      ;; Node data should include type info
      (is (search "type" json)))))

;;; --- HTML page contains required UI elements ---

(test html-contains-label-toggle
  "HTML page has label visibility toggle"
  (let ((html (ariadne::graph-page-html)))
    (is (search "label" html))
    ;; Should have a control for label mode
    (is (search "labelMode" html))))

(test html-contains-edge-label-toggle
  "HTML page has edge label toggle"
  (let ((html (ariadne::graph-page-html)))
    (is (search "edgeLabel" html))))
