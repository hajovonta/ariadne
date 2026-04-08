;;;; tests/suite-traversal.lisp
;;;; Gremlin-like graph traversal API

(in-package #:ariadne/tests)
(in-suite :traversal)

;; =============================================================================
;; Basic Traversal Steps
;; =============================================================================

(test traverse-out
  "Traverse outgoing edges"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (add-triple g "bob" "knows" "dave")
    (let ((results (traverse g "alice" '(out "knows"))))
      (is (= 2 (length results)))
      (is-true (member "bob" results :test #'equal))
      (is-true (member "charlie" results :test #'equal)))))

(test traverse-in
  "Traverse incoming edges"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "charlie" "knows" "bob")
    (let ((results (traverse g "bob" '(in "knows"))))
      (is (= 2 (length results))))))

(test traverse-both
  "Traverse edges in both directions"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "charlie" "knows" "alice")
    (let ((results (traverse g "alice" '(both "knows"))))
      (is (= 2 (length results))))))

;; =============================================================================
;; Chained Traversal
;; =============================================================================

(test traverse-chain-two-hops
  "Chain two traversal steps (friend of friend)"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (add-triple g "bob" "knows" "dave")
    (let ((results (traverse g "alice" '(out "knows") '(out "knows"))))
      (is (= 2 (length results)))
      (is-true (member "charlie" results :test #'equal))
      (is-true (member "dave" results :test #'equal)))))

(test traverse-chain-three-hops
  "Chain three traversal steps"
  (let ((g (make-graph)))
    (add-triple g "a" "next" "b")
    (add-triple g "b" "next" "c")
    (add-triple g "c" "next" "d")
    (let ((results (traverse g "a" '(out "next") '(out "next") '(out "next"))))
      (is (= 1 (length results)))
      (is (equal "d" (first results))))))

;; =============================================================================
;; Traversal with Filters
;; =============================================================================

(test traverse-with-has-filter
  "Filter traversal results by property"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (add-triple g "bob" "age" 25)
    (add-triple g "charlie" "age" 35)
    (let ((results (traverse g "alice"
                             '(out "knows")
                             '(has "age" (> 30)))))
      (is (= 1 (length results)))
      (is (equal "charlie" (first results))))))

(test traverse-with-type-filter
  "Filter by node type/label"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "acme")
    (add-triple g "bob" "type" "person")
    (add-triple g "acme" "type" "company")
    (let ((results (traverse g "alice"
                             '(out "knows")
                             '(has "type" "person"))))
      (is (= 1 (length results))))))

;; =============================================================================
;; Values Extraction
;; =============================================================================

(test traverse-values
  "Extract property values from traversal results"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (add-triple g "bob" "age" 25)
    (add-triple g "charlie" "age" 35)
    (let ((results (traverse g "alice"
                             '(out "knows")
                             '(values "age"))))
      (is (= 2 (length results)))
      (is-true (member 25 results))
      (is-true (member 35 results)))))

;; =============================================================================
;; Path Tracking
;; =============================================================================

(test traverse-path
  "Track the path taken during traversal"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (let ((paths (traverse-with-path g "alice" '(out "knows") '(out "knows"))))
      (is (= 1 (length paths)))
      (is (equal '("alice" "bob" "charlie") (first paths))))))

;; =============================================================================
;; Cycle Detection
;; =============================================================================

(test traverse-cycle-detection
  "Traversal should handle cycles without infinite loops"
  (let ((g (make-graph)))
    (add-triple g "a" "next" "b")
    (add-triple g "b" "next" "c")
    (add-triple g "c" "next" "a")
    (let ((results (traverse g "a" '(out "next") '(out "next") '(out "next"))))
      ;; Should terminate and return "a" (back to start)
      (is (= 1 (length results))))))

;; =============================================================================
;; Depth-Limited Traversal
;; =============================================================================

(test traverse-depth-limited
  "Traverse up to a maximum depth"
  (let ((g (make-graph)))
    (add-triple g "a" "next" "b")
    (add-triple g "b" "next" "c")
    (add-triple g "c" "next" "d")
    (add-triple g "d" "next" "e")
    (let ((results (traverse-depth g "a" "next" :max-depth 2)))
      ;; Should find b and c but not d or e
      (is-true (member "b" results :test #'equal))
      (is-true (member "c" results :test #'equal))
      (is-false (member "d" results :test #'equal)))))

;; =============================================================================
;; Shortest Path
;; =============================================================================

(test shortest-path
  "Find shortest path between two nodes"
  (let ((g (make-graph)))
    (add-triple g "a" "connects" "b")
    (add-triple g "b" "connects" "c")
    (add-triple g "a" "connects" "c")  ; direct shortcut
    (let ((path (shortest-path g "a" "c" :edge-type "connects")))
      (is (= 2 (length path)))  ; a -> c directly
      (is (equal "a" (first path)))
      (is (equal "c" (second path))))))

(test shortest-path-no-path
  "Shortest path returns nil when no path exists"
  (let ((g (make-graph)))
    (add-triple g "a" "connects" "b")
    (add-triple g "c" "connects" "d")
    (is-false (shortest-path g "a" "d" :edge-type "connects"))))
