;;;; tests/suite-delete-where.lisp
;;;; Tests for DELETE WHERE and SPARQL DESCRIBE parsing

(in-package #:ariadne/tests)

(in-suite :delete-where)

(test delete-where-basic
  "DELETE WHERE removes triples matching a pattern"
  (let ((g (make-graph :name "dw-test")))
    (add-triple g "http://ex.org/alice" "http://ex.org/knows" "http://ex.org/bob")
    (add-triple g "http://ex.org/alice" "http://ex.org/age" "30")
    (add-triple g "http://ex.org/bob" "http://ex.org/knows" "http://ex.org/carol")
    (sparql-update g "DELETE WHERE { <http://ex.org/alice> ?p ?o }")
    (is (= 1 (triple-count g)))
    (is (has-triple-p g "http://ex.org/bob" "http://ex.org/knows" "http://ex.org/carol"))))

(test delete-where-with-prefix
  "DELETE WHERE works with PREFIX declarations"
  (let ((g (make-graph :name "dw-prefix")))
    (add-triple g "http://ex.org/alice" "http://ex.org/knows" "http://ex.org/bob")
    (add-triple g "http://ex.org/alice" "http://ex.org/age" "30")
    (sparql-update g "PREFIX ex: <http://ex.org/>
DELETE WHERE { ex:alice ex:knows ?o }")
    (is (= 1 (triple-count g)))
    (is (has-triple-p g "http://ex.org/alice" "http://ex.org/age" "30"))))

(test delete-where-no-match
  "DELETE WHERE with no matches leaves graph unchanged"
  (let ((g (make-graph :name "dw-nomatch")))
    (add-triple g "http://ex.org/a" "http://ex.org/b" "http://ex.org/c")
    (sparql-update g "DELETE WHERE { <http://ex.org/x> ?p ?o }")
    (is (= 1 (triple-count g)))))

(test delete-where-all-variables
  "DELETE WHERE { ?s ?p ?o } clears the graph"
  (let ((g (make-graph :name "dw-all")))
    (add-triple g "http://ex.org/a" "http://ex.org/b" "http://ex.org/c")
    (add-triple g "http://ex.org/d" "http://ex.org/e" "http://ex.org/f")
    (sparql-update g "DELETE WHERE { ?s ?p ?o }")
    (is (= 0 (triple-count g)))))

(test sparql-describe-basic
  "SPARQL DESCRIBE returns triples about a resource"
  (let ((g (make-graph :name "desc-test")))
    (add-triple g "http://ex.org/alice" "http://ex.org/knows" "http://ex.org/bob")
    (add-triple g "http://ex.org/bob" "http://ex.org/knows" "http://ex.org/alice")
    (add-triple g "http://ex.org/carol" "http://ex.org/age" "30")
    (let ((results (sparql g "DESCRIBE <http://ex.org/alice>")))
      (is (= 2 (length results))))))

(test sparql-describe-with-prefix
  "SPARQL DESCRIBE works with PREFIX"
  (let ((g (make-graph :name "desc-prefix")))
    (add-triple g "http://ex.org/alice" "http://ex.org/knows" "http://ex.org/bob")
    (let ((results (sparql g "PREFIX ex: <http://ex.org/>
DESCRIBE ex:alice")))
      (is (= 1 (length results))))))
