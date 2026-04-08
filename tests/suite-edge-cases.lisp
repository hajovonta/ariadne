;;;; tests/suite-edge-cases.lisp
;;;; Edge cases, empty graphs, unicode, stress tests

(in-package #:ariadne/tests)
(in-suite :edge-cases)

;; =============================================================================
;; Empty Graph Operations
;; =============================================================================

(test empty-graph-triple-count
  "Empty graph has zero triples"
  (is (= 0 (triple-count (make-graph)))))

(test empty-graph-query
  "Query on empty graph returns empty results"
  (let ((g (make-graph)))
    (is (= 0 (length (get-triples g :subject "alice"))))))

(test empty-graph-remove
  "Remove on empty graph is a no-op"
  (let ((g (make-graph)))
    (finishes (remove-triple g "alice" "knows" "bob"))))

(test empty-graph-all-subjects
  "all-subjects on empty graph returns empty list"
  (is (null (all-subjects (make-graph)))))

;; =============================================================================
;; Unicode Support
;; =============================================================================

(test unicode-subjects
  "Unicode strings as subjects"
  (let ((g (make-graph)))
    (add-triple g "アリス" "knows" "ボブ")
    (is (= 1 (triple-count g)))
    (is-true (has-triple-p g "アリス" "knows" "ボブ"))))

(test unicode-predicates
  "Unicode strings as predicates"
  (let ((g (make-graph)))
    (add-triple g "alice" "知っている" "bob")
    (is (= 1 (length (get-triples g :predicate "知っている"))))))

(test unicode-objects
  "Unicode strings as objects"
  (let ((g (make-graph)))
    (add-triple g "alice" "name" "Alizée")
    (is-true (has-triple-p g "alice" "name" "Alizée"))))

(test emoji-in-triples
  "Emoji characters in triples"
  (let ((g (make-graph)))
    (add-triple g "alice" "mood" "😊")
    (is-true (has-triple-p g "alice" "mood" "😊"))))

;; =============================================================================
;; Special Values
;; =============================================================================

(test nil-handling
  "NIL as a triple component should be handled gracefully"
  (let ((g (make-graph)))
    (signals error (add-triple g nil "knows" "bob"))
    (signals error (add-triple g "alice" nil "bob"))
    ;; NIL as object might be valid (representing absence)
    ;; depending on design decision
    ))

(test empty-string-components
  "Empty strings as triple components"
  (let ((g (make-graph)))
    (add-triple g "" "knows" "bob")
    (is (= 1 (triple-count g)))
    (is-true (has-triple-p g "" "knows" "bob"))))

(test very-long-strings
  "Very long strings as triple components"
  (let ((g (make-graph))
        (long-string (make-string 10000 :initial-element #\x)))
    (add-triple g long-string "has-data" "value")
    (is (= 1 (triple-count g)))
    (is-true (has-triple-p g long-string "has-data" "value"))))

;; =============================================================================
;; Duplicate and Idempotency
;; =============================================================================

(test add-remove-add
  "Add, remove, then add the same triple again"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (remove-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "bob")
    (is (= 1 (triple-count g)))))

(test remove-idempotent
  "Removing the same triple twice is safe"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (remove-triple g "alice" "knows" "bob")
    (finishes (remove-triple g "alice" "knows" "bob"))
    (is (= 0 (triple-count g)))))

;; =============================================================================
;; Mixed Types
;; =============================================================================

(test mixed-type-objects
  "Different types as objects for the same subject-predicate"
  (let ((g (make-graph)))
    (add-triple g "alice" "data" 42)
    (add-triple g "alice" "data" "forty-two")
    (add-triple g "alice" "data" :forty-two)
    (is (= 3 (triple-count g)))))

(test number-equality
  "Integer and float with same value are distinct"
  (let ((g (make-graph)))
    (add-triple g "alice" "score" 42)
    (add-triple g "alice" "score" 42.0)
    ;; These should be distinct triples (different types)
    (is (= 2 (triple-count g)))))

;; =============================================================================
;; Stress / Scale
;; =============================================================================

(test many-subjects
  "Graph with many distinct subjects"
  (let ((g (make-graph)))
    (dotimes (i 1000)
      (add-triple g (format nil "person-~A" i) "type" "person"))
    (is (= 1000 (triple-count g)))
    (is (= 1000 (length (all-subjects g))))))

(test star-topology
  "Star graph: one center node connected to many"
  (let ((g (make-graph)))
    (dotimes (i 500)
      (add-triple g "center" "connects" (format nil "leaf-~A" i)))
    (is (= 500 (length (get-triples g :subject "center"))))))

(test complete-graph-small
  "Small complete graph (every node connects to every other)"
  (let ((g (make-graph))
        (nodes '("a" "b" "c" "d" "e")))
    (dolist (from nodes)
      (dolist (to nodes)
        (unless (equal from to)
          (add-triple g from "connects" to))))
    ;; 5 nodes, each connects to 4 others = 20 edges
    (is (= 20 (triple-count g)))))
