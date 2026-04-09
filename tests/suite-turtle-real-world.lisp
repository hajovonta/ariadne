;;;; tests/suite-turtle-real-world.lisp
;;;; Real-world Turtle parsing: 'a' shorthand, comments, booleans, inline semicolons

(in-package #:ariadne/tests)
(in-suite :turtle-real-world)

;; =============================================================================
;; 'a' shorthand for rdf:type
;; =============================================================================

(test turtle-a-shorthand
  "'a' is parsed as rdf:type"
  (let ((g (make-graph)))
    (import-turtle g "@prefix ex: <http://example.org/> .
ex:alice a ex:Person .")
    (is (= 1 (triple-count g)))
    (is-true (has-triple-p g "http://example.org/alice"
                             "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                             "http://example.org/Person"))))

(test turtle-a-with-semicolons
  "'a' combined with semicolon shorthand on same line"
  (let ((g (make-graph)))
    (import-turtle g "@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
ex:Dog a rdfs:Class ; rdfs:label \"Dog\" .")
    (is (= 2 (triple-count g)))))

;; =============================================================================
;; Comments
;; =============================================================================

(test turtle-comments-ignored
  "Lines starting with # are ignored"
  (let ((g (make-graph)))
    (import-turtle g "@prefix ex: <http://example.org/> .
# This is a comment
ex:alice ex:knows ex:bob .
# Another comment
ex:bob ex:knows ex:charlie .")
    (is (= 2 (triple-count g)))))

(test turtle-inline-comments
  "Comments after data on the same line"
  (let ((g (make-graph)))
    (import-turtle g "@prefix ex: <http://example.org/> .
ex:alice ex:age 30 . # alice's age")
    (is (= 1 (triple-count g)))))

;; =============================================================================
;; Boolean literals
;; =============================================================================

(test turtle-boolean-true
  "true is parsed as boolean T"
  (let ((g (make-graph)))
    (import-turtle g "@prefix ex: <http://example.org/> .
ex:feature ex:enabled true .")
    (let ((triples (get-triples g :subject "http://example.org/feature")))
      (is (= 1 (length triples)))
      (is (eq t (triple-object (first triples)))))))

(test turtle-boolean-false
  "false is parsed as boolean NIL"
  (let ((g (make-graph)))
    (import-turtle g "@prefix ex: <http://example.org/> .
ex:feature ex:enabled false .")
    (let ((triples (get-triples g :subject "http://example.org/feature")))
      (is (= 1 (length triples)))
      (is (eq nil (triple-object (first triples)))))))

;; =============================================================================
;; Comma-separated string literals
;; =============================================================================

(test turtle-comma-strings
  "Comma-separated string objects"
  (let ((g (make-graph)))
    (import-turtle g "@prefix ex: <http://example.org/> .
ex:project ex:dep \"lib-a\", \"lib-b\", \"lib-c\" .")
    (is (= 3 (triple-count g)))))

;; =============================================================================
;; Complex single-line patterns
;; =============================================================================

(test turtle-multi-semicolon-single-line
  "Multiple semicolons on a single line"
  (let ((g (make-graph)))
    (import-turtle g "@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
ex:Cat a rdfs:Class ; rdfs:label \"Cat\" ; ex:legs 4 .")
    (is (= 3 (triple-count g)))))

;; =============================================================================
;; Real-world file import
;; =============================================================================

(test turtle-import-perihelion-kg
  "Import the perihelion knowledge graph TTL file"
  (let ((g (make-graph))
        (path (probe-file #p"~/quicklisp/local-projects/perihelion/perihelion-kg.ttl")))
    (when path
      (import-turtle g (uiop:read-file-string path))
      ;; Should have a substantial number of triples
      (is (> (triple-count g) 100))
      ;; Spot-check: system entity should exist
      (let ((sys-triples (get-triples g :subject "http://perihelion.game/system/perihelion")))
        (is (> (length sys-triples) 0)))
      ;; Spot-check: rdf:type triples should exist (from 'a' shorthand)
      (let ((type-triples (get-triples g :predicate "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")))
        (is (> (length type-triples) 50))))))
