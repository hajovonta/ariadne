;;;; tests/suite-triple-store.lisp
;;;; Core triple store operations

(in-package #:ariadne/tests)
(in-suite :triple-store)

;; =============================================================================
;; Graph Creation
;; =============================================================================

(test make-graph
  "Create an empty graph"
  (let ((g (make-graph)))
    (is-true (graphp g))
    (is (= 0 (triple-count g)))))

(test make-named-graph
  "Create a named graph"
  (let ((g (make-graph :name "test-graph")))
    (is (string= "test-graph" (graph-name g)))))

;; =============================================================================
;; Adding Triples
;; =============================================================================

(test add-triple-basic
  "Add a single triple"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (is (= 1 (triple-count g)))))

(test add-triple-returns-triple
  "add-triple returns the triple object"
  (let ((g (make-graph)))
    (let ((tr (add-triple g "alice" "knows" "bob")))
      (is-true (triplep tr))
      (is (equal "alice" (triple-subject tr)))
      (is (equal "knows" (triple-predicate tr)))
      (is (equal "bob" (triple-object tr))))))

(test add-multiple-triples
  "Add multiple triples"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (add-triple g "alice" "age" 30)
    (is (= 3 (triple-count g)))))

(test add-duplicate-triple
  "Adding the same triple twice should not create duplicates"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "bob")
    (is (= 1 (triple-count g)))))

(test add-triple-with-symbols
  "Triples can use symbols as subject/predicate/object"
  (let ((g (make-graph)))
    (add-triple g :alice :knows :bob)
    (is (= 1 (triple-count g)))))

(test add-triple-with-numbers
  "Triples can use numbers as objects"
  (let ((g (make-graph)))
    (add-triple g "alice" "age" 30)
    (add-triple g "alice" "height" 1.65)
    (is (= 2 (triple-count g)))))

(test add-triple-with-uri
  "Triples can use URI-like strings"
  (let ((g (make-graph)))
    (add-triple g "http://example.org/alice"
                  "http://xmlns.com/foaf/0.1/knows"
                  "http://example.org/bob")
    (is (= 1 (triple-count g)))))

;; =============================================================================
;; Removing Triples
;; =============================================================================

(test remove-triple-basic
  "Remove a specific triple"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (remove-triple g "alice" "knows" "bob")
    (is (= 1 (triple-count g)))))

(test remove-nonexistent-triple
  "Removing a triple that doesn't exist is a no-op"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (remove-triple g "alice" "knows" "charlie")
    (is (= 1 (triple-count g)))))

(test remove-by-subject
  "Remove all triples with a given subject"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "age" 30)
    (add-triple g "bob" "knows" "charlie")
    (remove-triples g :subject "alice")
    (is (= 1 (triple-count g)))))

(test remove-by-predicate
  "Remove all triples with a given predicate"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (add-triple g "alice" "age" 30)
    (remove-triples g :predicate "knows")
    (is (= 1 (triple-count g)))))

(test clear-graph
  "Clear all triples from a graph"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (clear-graph g)
    (is (= 0 (triple-count g)))))

;; =============================================================================
;; Querying Triples
;; =============================================================================

(test query-by-subject
  "Find all triples with a given subject"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "age" 30)
    (add-triple g "bob" "knows" "charlie")
    (let ((results (get-triples g :subject "alice")))
      (is (= 2 (length results))))))

(test query-by-predicate
  "Find all triples with a given predicate"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (add-triple g "alice" "age" 30)
    (let ((results (get-triples g :predicate "knows")))
      (is (= 2 (length results))))))

(test query-by-object
  "Find all triples with a given object"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "charlie" "knows" "bob")
    (add-triple g "alice" "knows" "dave")
    (let ((results (get-triples g :object "bob")))
      (is (= 2 (length results))))))

(test query-by-subject-predicate
  "Find triples matching subject AND predicate"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (add-triple g "alice" "age" 30)
    (let ((results (get-triples g :subject "alice" :predicate "knows")))
      (is (= 2 (length results))))))

(test query-exact-triple
  "Check if a specific triple exists"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (is-true (has-triple-p g "alice" "knows" "bob"))
    (is-false (has-triple-p g "alice" "knows" "charlie"))))

(test query-all-triples
  "Get all triples in the graph"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (is (= 2 (length (get-triples g))))))

;; =============================================================================
;; Subjects, Predicates, Objects enumeration
;; =============================================================================

(test all-subjects
  "Get all unique subjects"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "age" 30)
    (add-triple g "bob" "knows" "charlie")
    (is (= 2 (length (all-subjects g))))))

(test all-predicates
  "Get all unique predicates"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "age" 30)
    (is (= 2 (length (all-predicates g))))))

(test all-objects
  "Get all unique objects"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (add-triple g "alice" "age" 30)
    (is (= 3 (length (all-objects g))))))

;; =============================================================================
;; remove-triples additional combinations
;; =============================================================================

(test remove-by-object
  "Remove all triples with a given object"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "charlie" "knows" "bob")
    (add-triple g "alice" "knows" "dave")
    (remove-triples g :object "bob")
    (is (= 1 (triple-count g)))
    (is-true (has-triple-p g "alice" "knows" "dave"))))

(test remove-by-subject-predicate
  "Remove triples matching subject AND predicate"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (add-triple g "alice" "age" 30)
    (remove-triples g :subject "alice" :predicate "knows")
    (is (= 1 (triple-count g)))
    (is-true (has-triple-p g "alice" "age" 30))))

;; =============================================================================
;; get-triples subject+object (OSP path)
;; =============================================================================

(test query-by-subject-object
  "Find triples matching subject AND object (uses OSP index)"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "likes" "bob")
    (add-triple g "alice" "knows" "charlie")
    (let ((results (get-triples g :subject "alice" :object "bob")))
      (is (= 2 (length results))))))
