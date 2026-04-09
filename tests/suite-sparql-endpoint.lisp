;;;; tests/suite-sparql-endpoint.lisp
;;;; SPARQL HTTP endpoint tests

(in-package #:ariadne/tests)
(in-suite :sparql-endpoint)

(test sparql-endpoint-json-results
  "SPARQL query returns JSON results"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (let* ((ariadne::*web-graph* g)
           (result (ariadne::sparql-query-json g "SELECT ?x WHERE { \"alice\" \"knows\" ?x }")))
      (is (stringp result))
      (is (search "bob" result)))))

(test sparql-endpoint-ask-true
  "SPARQL ASK returns boolean true"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (let ((result (ariadne::sparql-query-json g "ASK { \"alice\" \"knows\" \"bob\" }")))
      (is (search "true" result)))))

(test sparql-endpoint-ask-false
  "SPARQL ASK returns boolean false"
  (let ((g (make-graph)))
    (let ((result (ariadne::sparql-query-json g "ASK { \"alice\" \"knows\" \"bob\" }")))
      (is (search "false" result)))))

(test sparql-endpoint-error
  "SPARQL endpoint returns error on bad query"
  (let ((g (make-graph)))
    (let ((result (ariadne::sparql-query-json g "INVALID QUERY")))
      (is (search "error" result)))))

(test sparql-endpoint-server-lifecycle
  "Server starts and stops with SPARQL endpoint"
  (let ((g (make-graph)))
    (add-triple g "a" "b" "c")
    (ariadne::start-web-server g :port 18766)
    (sleep 0.1)
    (is-true ariadne::*web-server*)
    (ariadne::stop-web-server)
    (is (null ariadne::*web-server*))))
