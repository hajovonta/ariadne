;;;; tests/suite-turtle-export.lisp
;;;; Proper Turtle export with prefix grouping and shorthand

(in-package #:ariadne/tests)
(in-suite :turtle-export)

;; =============================================================================
;; Basic Turtle Export
;; =============================================================================

(test export-turtle-prefixes
  "Turtle export detects common prefixes and emits @prefix declarations"
  (let ((g (make-graph)))
    (add-triple g "http://example.org/alice" "http://example.org/knows" "http://example.org/bob")
    (let ((ttl (export-turtle g)))
      (is (search "@prefix" ttl))
      (is (search "ex:" ttl)))))

(test export-turtle-semicolons
  "Turtle export uses semicolon shorthand for same subject"
  (let ((g (make-graph)))
    (add-triple g "http://example.org/alice" "http://example.org/name" "Alice")
    (add-triple g "http://example.org/alice" "http://example.org/age" 30)
    (let ((ttl (export-turtle g)))
      (is (search ";" ttl)))))

(test export-turtle-commas
  "Turtle export uses comma shorthand for same subject+predicate"
  (let ((g (make-graph)))
    (add-triple g "http://example.org/alice" "http://example.org/knows" "http://example.org/bob")
    (add-triple g "http://example.org/alice" "http://example.org/knows" "http://example.org/charlie")
    (let ((ttl (export-turtle g)))
      (is (search "," ttl)))))

(test export-turtle-roundtrip
  "Export then import preserves all triples"
  (let ((g1 (make-graph))
        (g2 (make-graph)))
    (add-triple g1 "http://example.org/alice" "http://example.org/knows" "http://example.org/bob")
    (add-triple g1 "http://example.org/alice" "http://example.org/name" "Alice")
    (add-triple g1 "http://example.org/bob" "http://example.org/name" "Bob")
    (let ((ttl (export-turtle g1)))
      (import-turtle g2 ttl))
    (is (= (triple-count g1) (triple-count g2)))))

(test export-turtle-a-shorthand
  "Turtle export uses 'a' for rdf:type"
  (let ((g (make-graph)))
    (add-triple g "http://example.org/alice"
                  "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                  "http://example.org/Person")
    (let ((ttl (export-turtle g)))
      (is (search " a " ttl)))))

(test export-turtle-literals
  "Turtle export handles string and numeric literals"
  (let ((g (make-graph)))
    (add-triple g "http://example.org/alice" "http://example.org/name" "Alice")
    (add-triple g "http://example.org/alice" "http://example.org/age" 30)
    (let ((ttl (export-turtle g)))
      (is (search "\"Alice\"" ttl))
      (is (search "30" ttl)))))
