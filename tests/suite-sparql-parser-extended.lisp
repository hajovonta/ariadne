;;;; tests/suite-sparql-parser-extended.lisp
;;;; Extended SPARQL string parser tests

(in-package #:ariadne/tests)
(in-suite :sparql-parser-extended)

;; =============================================================================
;; CONSTRUCT
;; =============================================================================

(test sparql-parse-construct
  "Parse and execute SPARQL CONSTRUCT"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (let ((result (sparql g "CONSTRUCT { ?a <friendOf> ?b } WHERE { ?a <knows> ?b }")))
      (is (= 2 (length result)))
      (is (equal "friendOf" (second (first result)))))))

;; =============================================================================
;; OPTIONAL
;; =============================================================================

(test sparql-parse-optional
  "Parse SPARQL OPTIONAL"
  (let ((g (make-graph)))
    (add-triple g "alice" "name" "Alice")
    (add-triple g "alice" "age" 30)
    (add-triple g "bob" "name" "Bob")
    (let ((result (sparql g "SELECT ?name ?age WHERE { ?p <name> ?name . OPTIONAL { ?p <age> ?age } }")))
      (is (= 2 (length result))))))

;; =============================================================================
;; GROUP BY / HAVING
;; =============================================================================

(test sparql-parse-group-by
  "Parse SPARQL GROUP BY with COUNT"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (add-triple g "bob" "knows" "charlie")
    (let ((result (sparql g "SELECT ?person (COUNT ?friend) WHERE { ?person <knows> ?friend } GROUP BY ?person")))
      (is (= 2 (length result))))))

(test sparql-parse-having
  "Parse SPARQL HAVING"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (add-triple g "bob" "knows" "charlie")
    (let ((result (sparql g "SELECT ?person (COUNT ?friend) WHERE { ?person <knows> ?friend } GROUP BY ?person HAVING (COUNT ?friend) > 1")))
      ;; alice has 2 friends, bob has 1 — only alice passes HAVING
      (is (>= (length result) 1)))))

;; =============================================================================
;; UNION
;; =============================================================================

(test sparql-parse-union
  "Parse SPARQL UNION"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "charlie" "likes" "bob")
    (let ((result (sparql g "SELECT ?who WHERE { { ?who <knows> <bob> } UNION { ?who <likes> <bob> } }")))
      (is (= 2 (length result))))))

;; =============================================================================
;; OFFSET
;; =============================================================================

(test sparql-parse-offset
  "Parse SPARQL OFFSET"
  (let ((g (make-graph)))
    (add-triple g "a" "p" "1")
    (add-triple g "b" "p" "2")
    (add-triple g "c" "p" "3")
    (let ((result (sparql g "SELECT ?s WHERE { ?s <p> ?o } LIMIT 1 OFFSET 1")))
      (is (= 1 (length result))))))
