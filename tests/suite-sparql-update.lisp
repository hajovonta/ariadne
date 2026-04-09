;;;; tests/suite-sparql-update.lisp
;;;; SPARQL UPDATE: INSERT DATA, DELETE DATA

(in-package #:ariadne/tests)
(in-suite :sparql-update)

(test sparql-insert-data
  "INSERT DATA adds triples"
  (let ((g (make-graph)))
    (sparql-update g "INSERT DATA { <http://ex.org/alice> <http://ex.org/knows> <http://ex.org/bob> . }")
    (is (= 1 (triple-count g)))
    (is-true (has-triple-p g "http://ex.org/alice" "http://ex.org/knows" "http://ex.org/bob"))))

(test sparql-insert-data-multiple
  "INSERT DATA with multiple triples"
  (let ((g (make-graph)))
    (sparql-update g "INSERT DATA {
      <http://ex.org/alice> <http://ex.org/knows> <http://ex.org/bob> .
      <http://ex.org/alice> <http://ex.org/name> \"Alice\" .
    }")
    (is (= 2 (triple-count g)))))

(test sparql-insert-data-prefix
  "INSERT DATA with PREFIX"
  (let ((g (make-graph)))
    (sparql-update g "PREFIX ex: <http://ex.org/>
      INSERT DATA { ex:alice ex:knows ex:bob . }")
    (is (= 1 (triple-count g)))))

(test sparql-delete-data
  "DELETE DATA removes triples"
  (let ((g (make-graph)))
    (add-triple g "http://ex.org/alice" "http://ex.org/knows" "http://ex.org/bob")
    (add-triple g "http://ex.org/alice" "http://ex.org/name" "Alice")
    (sparql-update g "DELETE DATA { <http://ex.org/alice> <http://ex.org/knows> <http://ex.org/bob> . }")
    (is (= 1 (triple-count g)))
    (is-false (has-triple-p g "http://ex.org/alice" "http://ex.org/knows" "http://ex.org/bob"))))

(test sparql-delete-data-prefix
  "DELETE DATA with PREFIX"
  (let ((g (make-graph)))
    (add-triple g "http://ex.org/alice" "http://ex.org/knows" "http://ex.org/bob")
    (sparql-update g "PREFIX ex: <http://ex.org/>
      DELETE DATA { ex:alice ex:knows ex:bob . }")
    (is (= 0 (triple-count g)))))

(test sparql-update-via-endpoint
  "SPARQL UPDATE works through JSON endpoint"
  (let ((g (make-graph)))
    (let ((result (ariadne::sparql-update-json g "INSERT DATA { <http://ex.org/a> <http://ex.org/b> <http://ex.org/c> . }")))
      (is (search "success" result))
      (is (= 1 (triple-count g))))))
