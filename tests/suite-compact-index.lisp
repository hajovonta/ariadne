;;;; tests/suite-compact-index.lisp
;;;; Compact index: verify correctness after index restructuring

(in-package #:ariadne/tests)
(in-suite :compact-index)

;; =============================================================================
;; These tests verify that all existing functionality works with compact indexes.
;; They duplicate key tests from other suites to ensure the index change
;; doesn't break anything.
;; =============================================================================

(test compact-add-and-count
  "Basic add and count with compact index"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (add-triple g "alice" "age" 30)
    (is (= 3 (triple-count g)))))

(test compact-dedup
  "Duplicates still deduplicated"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "bob")
    (is (= 1 (triple-count g)))))

(test compact-remove
  "Remove works correctly"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (remove-triple g "alice" "knows" "bob")
    (is (= 1 (triple-count g)))
    (is-false (has-triple-p g "alice" "knows" "bob"))
    (is-true (has-triple-p g "alice" "knows" "charlie"))))

(test compact-query-by-subject
  "Query by subject"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "age" 30)
    (add-triple g "bob" "knows" "charlie")
    (is (= 2 (length (get-triples g :subject "alice"))))))

(test compact-query-by-predicate
  "Query by predicate"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (add-triple g "alice" "age" 30)
    (is (= 2 (length (get-triples g :predicate "knows"))))))

(test compact-query-by-object
  "Query by object"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "charlie" "knows" "bob")
    (is (= 2 (length (get-triples g :object "bob"))))))

(test compact-query-two-keys
  "Query by subject + predicate"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (add-triple g "alice" "age" 30)
    (is (= 2 (length (get-triples g :subject "alice" :predicate "knows"))))))

(test compact-query-all
  "Query all triples"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "age" 25)
    (is (= 2 (length (get-triples g))))))

(test compact-clear
  "Clear empties all indexes"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "age" 25)
    (clear-graph g)
    (is (= 0 (triple-count g)))
    (is (= 0 (length (get-triples g :subject "alice"))))))

(test compact-memory-at-scale
  "Compact index handles 10K triples"
  (let ((g (make-graph)))
    (dotimes (i 10000)
      (add-triple g (format nil "s~A" i) "p" (format nil "o~A" i)))
    (is (= 10000 (triple-count g)))
    (is (= 1 (length (get-triples g :subject "s500"))))
    (is (= 1 (length (get-triples g :object "o500"))))))

(test compact-enumeration
  "all-subjects/predicates/objects work"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "age" 30)
    (add-triple g "bob" "knows" "charlie")
    (is (= 2 (length (all-subjects g))))
    (is (= 2 (length (all-predicates g))))
    (is (= 3 (length (all-objects g))))))
