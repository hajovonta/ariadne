;;;; tests/suite-json-ld-import.lisp
;;;; JSON-LD import

(in-package #:ariadne/tests)
(in-suite :json-ld-import)

(test json-ld-import-basic
  "Import basic JSON-LD"
  (let ((g (make-graph)))
    (import-json-ld g "[{\"@id\":\"http://ex.org/alice\",\"http://ex.org/knows\":{\"@id\":\"http://ex.org/bob\"}}]")
    (is (= 1 (triple-count g)))
    (is-true (has-triple-p g "http://ex.org/alice" "http://ex.org/knows" "http://ex.org/bob"))))

(test json-ld-import-literal
  "Import JSON-LD with literal values"
  (let ((g (make-graph)))
    (import-json-ld g "[{\"@id\":\"http://ex.org/alice\",\"http://ex.org/name\":\"Alice\"}]")
    (is (= 1 (triple-count g)))
    (is-true (has-triple-p g "http://ex.org/alice" "http://ex.org/name" "Alice"))))

(test json-ld-import-type
  "Import JSON-LD with @type"
  (let ((g (make-graph)))
    (import-json-ld g "[{\"@id\":\"http://ex.org/alice\",\"@type\":\"http://ex.org/Person\"}]")
    (is (= 1 (triple-count g)))
    (is-true (has-triple-p g "http://ex.org/alice"
                             "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                             "http://ex.org/Person"))))

(test json-ld-import-number
  "Import JSON-LD with numeric value"
  (let ((g (make-graph)))
    (import-json-ld g "[{\"@id\":\"http://ex.org/alice\",\"http://ex.org/age\":30}]")
    (is (= 1 (triple-count g)))
    (is-true (has-triple-p g "http://ex.org/alice" "http://ex.org/age" 30))))

(test json-ld-roundtrip
  "Export then import preserves triples"
  (let ((g1 (make-graph))
        (g2 (make-graph)))
    (add-triple g1 "http://ex.org/alice" "http://ex.org/knows" "http://ex.org/bob")
    (add-triple g1 "http://ex.org/alice" "http://ex.org/name" "Alice")
    (import-json-ld g2 (export-json-ld g1))
    (is (= (triple-count g1) (triple-count g2)))))
