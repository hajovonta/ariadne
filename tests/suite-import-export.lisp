;;;; tests/suite-import-export.lisp
;;;; N-Triples, Turtle, and N-Quads import/export

(in-package #:ariadne/tests)
(in-suite :import-export)

;; =============================================================================
;; N-Triples Import
;; =============================================================================

(test import-ntriples-basic
  "Import basic N-Triples format"
  (let ((g (make-graph))
        (data "<http://example.org/alice> <http://xmlns.com/foaf/0.1/knows> <http://example.org/bob> .
<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> \"Alice\" ."))
    (import-ntriples g data)
    (is (= 2 (triple-count g)))))

(test import-ntriples-literal-types
  "Import N-Triples with typed literals"
  (let ((g (make-graph))
        (data "<http://example.org/alice> <http://example.org/age> \"30\"^^<http://www.w3.org/2001/XMLSchema#integer> ."))
    (import-ntriples g data)
    (is (= 1 (triple-count g)))
    ;; The object should be parsed as integer 30
    (let ((triples (get-triples g :subject "http://example.org/alice")))
      (is (numberp (triple-object (first triples)))))))

(test import-ntriples-language-tags
  "Import N-Triples with language-tagged strings"
  (let ((g (make-graph))
        (data "<http://example.org/alice> <http://example.org/name> \"Alice\"@en .
<http://example.org/alice> <http://example.org/name> \"Alicia\"@es ."))
    (import-ntriples g data)
    (is (= 2 (triple-count g)))))

(test import-ntriples-blank-nodes
  "Import N-Triples with blank nodes"
  (let ((g (make-graph))
        (data "_:b1 <http://example.org/name> \"Anonymous\" .
<http://example.org/alice> <http://example.org/knows> _:b1 ."))
    (import-ntriples g data)
    (is (= 2 (triple-count g)))))

(test import-ntriples-from-file
  "Import N-Triples from a file"
  (let ((g (make-graph))
        (path (merge-pathnames "test-data/sample.nt"
                               (asdf:system-source-directory :ariadne-tests))))
    ;; This test requires the test-data/sample.nt file to exist
    (when (probe-file path)
      (import-ntriples-file g path)
      (is (> (triple-count g) 0)))))

;; =============================================================================
;; N-Triples Export
;; =============================================================================

(test export-ntriples-basic
  "Export graph as N-Triples"
  (let ((g (make-graph)))
    (add-triple g "http://example.org/alice"
                  "http://xmlns.com/foaf/0.1/knows"
                  "http://example.org/bob")
    (let ((output (export-ntriples g)))
      (is (stringp output))
      (is (search "alice" output))
      (is (search "knows" output))
      (is (search "bob" output))
      ;; Should end with " .\n"
      (is (search " ." output)))))

(test export-ntriples-roundtrip
  "Export then import should preserve all triples"
  (let ((g1 (make-graph))
        (g2 (make-graph)))
    (add-triple g1 "http://example.org/alice"
                   "http://xmlns.com/foaf/0.1/knows"
                   "http://example.org/bob")
    (add-triple g1 "http://example.org/alice"
                   "http://xmlns.com/foaf/0.1/name"
                   "Alice")
    (let ((nt (export-ntriples g1)))
      (import-ntriples g2 nt))
    (is (= (triple-count g1) (triple-count g2)))))

;; =============================================================================
;; Turtle Import
;; =============================================================================

(test import-turtle-basic
  "Import basic Turtle format"
  (let ((g (make-graph))
        (data "@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ex: <http://example.org/> .

ex:alice foaf:knows ex:bob .
ex:alice foaf:name \"Alice\" ."))
    (import-turtle g data)
    (is (= 2 (triple-count g)))))

(test import-turtle-prefix-expansion
  "Turtle prefixes are expanded to full URIs"
  (let ((g (make-graph))
        (data "@prefix ex: <http://example.org/> .
ex:alice ex:knows ex:bob ."))
    (import-turtle g data)
    (is-true (has-triple-p g "http://example.org/alice"
                             "http://example.org/knows"
                             "http://example.org/bob"))))

(test import-turtle-semicolon
  "Turtle semicolon shorthand (same subject)"
  (let ((g (make-graph))
        (data "@prefix ex: <http://example.org/> .
ex:alice ex:name \"Alice\" ;
         ex:age 30 ;
         ex:knows ex:bob ."))
    (import-turtle g data)
    (is (= 3 (triple-count g)))))

(test import-turtle-comma
  "Turtle comma shorthand (same subject and predicate)"
  (let ((g (make-graph))
        (data "@prefix ex: <http://example.org/> .
ex:alice ex:knows ex:bob , ex:charlie , ex:dave ."))
    (import-turtle g data)
    (is (= 3 (triple-count g)))))

;; =============================================================================
;; Turtle Export
;; =============================================================================

(test export-turtle-basic
  "Export graph as Turtle"
  (let ((g (make-graph)))
    (add-triple g "http://example.org/alice"
                  "http://example.org/knows"
                  "http://example.org/bob")
    (let ((output (export-turtle g)))
      (is (stringp output)))))

;; =============================================================================
;; N-Quads (Named Graphs)
;; =============================================================================

(test import-nquads
  "Import N-Quads format (triples with graph name)"
  (let ((g (make-graph))
        (data "<http://example.org/alice> <http://example.org/knows> <http://example.org/bob> <http://example.org/graph1> ."))
    (import-nquads g data)
    (is (= 1 (triple-count g)))))
