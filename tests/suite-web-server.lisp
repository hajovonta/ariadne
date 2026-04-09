;;;; tests/suite-web-server.lisp
;;;; Web visualization server tests

(in-package #:ariadne/tests)
(in-suite :web-server)

(test graph-to-cytoscape-json
  "Convert graph to Cytoscape.js JSON format"
  (let ((g (make-graph :name "test")))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (let ((json (ariadne::graph-to-cytoscape-json g)))
      (is (stringp json))
      (is (search "alice" json))
      (is (search "bob" json))
      (is (search "knows" json)))))

(test graph-to-cytoscape-json-empty
  "Empty graph produces valid JSON"
  (let* ((g (make-graph))
         (json (ariadne::graph-to-cytoscape-json g)))
    (is (stringp json))
    (is (search "[]" json))))

(test graph-to-cytoscape-json-with-filter
  "Filter by predicates"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "age" 30)
    (let ((json (ariadne::graph-to-cytoscape-json g :predicates '("knows"))))
      (is (search "knows" json))
      (is (not (search "age" json))))))

(test web-server-start-stop
  "Server starts and stops without error"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (ariadne::start-web-server g :port 18765)
    (sleep 0.1)
    (is-true ariadne::*web-server*)
    (ariadne::stop-web-server)
    (is (null ariadne::*web-server*))))
