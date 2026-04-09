;;;; tests/suite-export-formats.lisp
;;;; JSON-LD and Cytoscape JSON export

(in-package #:ariadne/tests)
(in-suite :export-formats)

(test export-cytoscape-json
  "Export graph as Cytoscape.js JSON"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (let ((json (export-cytoscape-json g)))
      (is (stringp json))
      (is (search "alice" json))
      (is (search "bob" json))
      (is (search "knows" json)))))

(test export-json-ld-basic
  "Export graph as JSON-LD"
  (let ((g (make-graph)))
    (add-triple g "http://example.org/alice" "http://example.org/knows" "http://example.org/bob")
    (add-triple g "http://example.org/alice" "http://example.org/name" "Alice")
    (let ((json (export-json-ld g)))
      (is (stringp json))
      (is (search "alice" json))
      (is (search "knows" json))
      (is (search "Alice" json)))))

(test export-json-ld-types
  "JSON-LD includes @type for rdf:type triples"
  (let ((g (make-graph)))
    (add-triple g "http://example.org/alice"
                "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                "http://example.org/Person")
    (let ((json (export-json-ld g)))
      (is (search "@type" json))
      (is (search "Person" json)))))

(test export-json-ld-empty
  "Empty graph produces valid JSON-LD"
  (let ((json (export-json-ld (make-graph))))
    (is (stringp json))
    (is (search "[]" json))))
