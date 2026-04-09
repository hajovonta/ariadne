;;;; tests/suite-error-handling.lisp
;;;; Graceful error handling for malformed input and bad queries

(in-package #:ariadne/tests)
(in-suite :error-handling)

;; =============================================================================
;; Import Errors
;; =============================================================================

(test error-malformed-ntriples
  "Malformed N-Triples line is skipped gracefully"
  (let ((g (make-graph)))
    (finishes
      (import-ntriples g "this is not valid ntriples
<http://example.org/a> <http://example.org/b> <http://example.org/c> ."))
    ;; The valid line should still be imported
    (is (= 1 (triple-count g)))))

(test error-empty-input
  "Empty input produces empty graph"
  (let ((g (make-graph)))
    (finishes (import-ntriples g ""))
    (is (= 0 (triple-count g)))))

(test error-turtle-unclosed-string
  "Turtle with unclosed string signals an error"
  (let ((g (make-graph)))
    (signals error
      (import-turtle g "@prefix ex: <http://example.org/> .
ex:a ex:b \"unclosed string ."))))

;; =============================================================================
;; Query Errors
;; =============================================================================

(test error-unknown-query-form
  "Unknown query form signals an error"
  (let ((g (make-graph)))
    (signals error
      (query g '(delete (?x) (where (?x ?p ?o)))))))

(test error-query-on-empty-graph
  "Query on empty graph returns empty results, not error"
  (let ((g (make-graph)))
    (finishes
      (let ((results (query g '(select (?x) (where (?x "knows" ?y))))))
        (is (= 0 (length results)))))))

;; =============================================================================
;; Persistence Errors
;; =============================================================================

(test error-load-nonexistent-file
  "Loading a nonexistent file signals an error"
  (signals error
    (load-graph #p"/tmp/nonexistent-ariadne-graph.ariadne")))

(test error-save-bad-path
  "Saving to an invalid path signals an error"
  (let ((g (make-graph)))
    (signals error
      (save-graph g #p"/nonexistent-directory/graph.ariadne"))))
