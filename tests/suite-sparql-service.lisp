;;;; tests/suite-sparql-service.lisp
;;;; Tests for SPARQL SERVICE (federated queries)

(in-package #:ariadne/tests)

(in-suite :sparql-service)

;; We test SERVICE by running a local SPARQL endpoint and querying it.
;; Start a temp web server on the graph, then query it via SERVICE.

(test service-parse
  "SPARQL parser handles SERVICE clause"
  (let* ((g (make-graph :name "svc-parse"))
         (expr (ariadne::parse-sparql
                "SELECT ?s ?name WHERE { ?s <http://ex.org/name> ?name . SERVICE <http://remote/sparql> { ?s <http://ex.org/age> ?age } }")))
    (declare (ignore g))
    (is (listp expr))
    ;; SERVICE should be inside the WHERE patterns
    (let ((where (find 'ariadne::where (cddr expr) :key #'first)))
      (is (find 'ariadne::service (rest where) :key #'first)))))

(test service-federated-query
  "SERVICE queries a remote SPARQL endpoint and joins results"
  (let ((local (make-graph :name "local"))
        (remote (make-graph :name "remote")))
    ;; Local has names
    (add-triple local "http://ex.org/alice" "http://ex.org/name" "Alice")
    (add-triple local "http://ex.org/bob" "http://ex.org/name" "Bob")
    ;; Remote has ages
    (add-triple remote "http://ex.org/alice" "http://ex.org/age" "30")
    (add-triple remote "http://ex.org/bob" "http://ex.org/age" "25")
    ;; Start remote endpoint
    (start-web-server remote :port 19876)
    (unwind-protect
         (let ((results (sparql-via-algebra local
                          "SELECT ?name ?age WHERE { ?s <http://ex.org/name> ?name . SERVICE <http://localhost:19876/sparql> { ?s <http://ex.org/age> ?age } }")))
           (is (= 2 (length results)))
           ;; Each result should have name and age
           (is (member "Alice" (mapcar #'first results) :test #'equal))
           (is (member "30" (mapcar #'second results) :test #'equal)))
      (stop-web-server))))

(test service-no-results
  "SERVICE with no matching results returns empty"
  (let ((local (make-graph :name "local-empty"))
        (remote (make-graph :name "remote-empty")))
    (add-triple local "http://ex.org/alice" "http://ex.org/name" "Alice")
    ;; Remote has nothing matching
    (add-triple remote "http://ex.org/carol" "http://ex.org/age" "40")
    (start-web-server remote :port 19877)
    (unwind-protect
         (let ((results (sparql-via-algebra local
                          "SELECT ?name ?age WHERE { ?s <http://ex.org/name> ?name . SERVICE <http://localhost:19877/sparql> { ?s <http://ex.org/age> ?age } }")))
           (is (= 0 (length results))))
      (stop-web-server))))
