;;;; tests/suite-indexing.lisp
;;;; SPO, POS, OSP index operations

(in-package #:ariadne/tests)
(in-suite :indexing)

;; =============================================================================
;; SPO Index (Subject -> Predicate -> Object)
;; =============================================================================

(test spo-lookup
  "SPO index: given subject, find all predicate-object pairs"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "age" 30)
    (add-triple g "bob" "knows" "charlie")
    (let ((results (get-triples g :subject "alice")))
      (is (= 2 (length results))))))

(test spo-lookup-nonexistent
  "SPO index: lookup for nonexistent subject returns empty"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (is (= 0 (length (get-triples g :subject "nobody"))))))

;; =============================================================================
;; POS Index (Predicate -> Object -> Subject)
;; =============================================================================

(test pos-lookup
  "POS index: given predicate, find all subject-object pairs"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (add-triple g "alice" "age" 30)
    (let ((results (get-triples g :predicate "knows")))
      (is (= 2 (length results))))))

(test pos-lookup-predicate-object
  "POS index: given predicate and object, find subjects"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "charlie" "knows" "bob")
    (add-triple g "dave" "knows" "alice")
    (let ((results (get-triples g :predicate "knows" :object "bob")))
      (is (= 2 (length results))))))

;; =============================================================================
;; OSP Index (Object -> Subject -> Predicate)
;; =============================================================================

(test osp-lookup
  "OSP index: given object, find all subject-predicate pairs"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "charlie" "likes" "bob")
    (add-triple g "alice" "knows" "dave")
    (let ((results (get-triples g :object "bob")))
      (is (= 2 (length results))))))

(test osp-lookup-object-subject
  "OSP index: given object and subject, find predicates"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "likes" "bob")
    (add-triple g "charlie" "knows" "bob")
    (let ((results (get-triples g :object "bob" :subject "alice")))
      (is (= 2 (length results))))))

;; =============================================================================
;; Index Consistency
;; =============================================================================

(test index-consistency-after-add
  "All three indexes are consistent after adding triples"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    ;; All three lookup paths should find the same triple
    (is (= 1 (length (get-triples g :subject "alice"))))
    (is (= 1 (length (get-triples g :predicate "knows"))))
    (is (= 1 (length (get-triples g :object "bob"))))))

(test index-consistency-after-remove
  "All three indexes are consistent after removing triples"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (remove-triple g "alice" "knows" "bob")
    (is (= 1 (length (get-triples g :subject "alice"))))
    (is (= 1 (length (get-triples g :predicate "knows"))))
    (is (= 0 (length (get-triples g :object "bob"))))
    (is (= 1 (length (get-triples g :object "charlie"))))))

(test index-consistency-after-clear
  "All indexes are empty after clearing the graph"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (clear-graph g)
    (is (= 0 (length (get-triples g :subject "alice"))))
    (is (= 0 (length (get-triples g :predicate "knows"))))
    (is (= 0 (length (get-triples g :object "bob"))))))

;; =============================================================================
;; Index Performance Characteristics
;; =============================================================================

(test index-many-triples
  "Indexes work correctly with many triples"
  (let ((g (make-graph)))
    (dotimes (i 100)
      (add-triple g (format nil "node-~A" i)
                    "connects-to"
                    (format nil "node-~A" (1+ i))))
    (is (= 100 (triple-count g)))
    ;; Each node (except first) is an object of exactly one triple
    (is (= 1 (length (get-triples g :object "node-50"))))
    ;; Each node (except last) is a subject of exactly one triple
    (is (= 1 (length (get-triples g :subject "node-50"))))))
