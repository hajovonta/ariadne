;;;; tests/suite-sparql-parser.lisp
;;;; Parse SPARQL query strings into Ariadne DSL expressions

(in-package #:ariadne/tests)
(in-suite :sparql-parser)

;; =============================================================================
;; Basic SELECT
;; =============================================================================

(test sparql-parse-simple-select
  "Parse a simple SELECT query"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (let ((results (sparql g "SELECT ?who WHERE { <alice> <knows> ?who }")))
      (is (= 1 (length results))))))

(test sparql-parse-two-variables
  "Parse SELECT with two variables"
  (let ((g (make-graph)))
    (add-triple g "alice" "age" 30)
    (add-triple g "bob" "age" 25)
    (let ((results (sparql g "SELECT ?person ?age WHERE { ?person <age> ?age }")))
      (is (= 2 (length results))))))

(test sparql-parse-join
  "Parse SELECT with join"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (let ((results (sparql g "SELECT ?fof WHERE { <alice> <knows> ?f . ?f <knows> ?fof }")))
      (is (= 1 (length results)))
      (is (equal "charlie" (caar results))))))

;; =============================================================================
;; PREFIX
;; =============================================================================

(test sparql-parse-prefix
  "Parse query with PREFIX declaration"
  (let ((g (make-graph)))
    (add-triple g "http://example.org/alice" "http://example.org/knows" "http://example.org/bob")
    (let ((results (sparql g "PREFIX ex: <http://example.org/>
                              SELECT ?who WHERE { ex:alice ex:knows ?who }")))
      (is (= 1 (length results))))))

;; =============================================================================
;; FILTER
;; =============================================================================

(test sparql-parse-filter
  "Parse SELECT with FILTER"
  (let ((g (make-graph)))
    (add-triple g "alice" "age" 30)
    (add-triple g "bob" "age" 25)
    (let ((results (sparql g "SELECT ?person WHERE { ?person <age> ?age FILTER(?age > 28) }")))
      (is (= 1 (length results))))))

;; =============================================================================
;; ASK
;; =============================================================================

(test sparql-parse-ask
  "Parse ASK query"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (is-true (sparql g "ASK { <alice> <knows> <bob> }"))
    (is-false (sparql g "ASK { <alice> <knows> <charlie> }"))))

;; =============================================================================
;; DISTINCT / LIMIT / ORDER BY
;; =============================================================================

(test sparql-parse-distinct
  "Parse SELECT DISTINCT"
  (let ((g (make-graph)))
    (add-triple g "alice" "type" "person")
    (add-triple g "bob" "type" "person")
    (add-triple g "acme" "type" "company")
    (let ((results (sparql g "SELECT DISTINCT ?type WHERE { ?x <type> ?type }")))
      (is (= 2 (length results))))))

(test sparql-parse-limit
  "Parse SELECT with LIMIT"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (add-triple g "alice" "knows" "dave")
    (let ((results (sparql g "SELECT ?who WHERE { <alice> <knows> ?who } LIMIT 2")))
      (is (= 2 (length results))))))
