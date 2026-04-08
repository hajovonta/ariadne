;;;; tests/suite-property-graph.lisp
;;;; Property graph model layered on top of triple store

(in-package #:ariadne/tests)
(in-suite :property-graph)

;; =============================================================================
;; Node Operations
;; =============================================================================

(test create-node
  "Create a node with an ID"
  (let ((g (make-graph)))
    (let ((n (add-node g "alice")))
      (is-true n)
      (is (equal "alice" (node-id n))))))

(test create-node-with-properties
  "Create a node with properties"
  (let ((g (make-graph)))
    (add-node g "alice" :properties '((:name . "Alice") (:age . 30)))
    (is (equal "Alice" (node-property g "alice" :name)))
    (is (= 30 (node-property g "alice" :age)))))

(test set-node-property
  "Set a property on an existing node"
  (let ((g (make-graph)))
    (add-node g "alice")
    (set-node-property g "alice" :name "Alice")
    (is (equal "Alice" (node-property g "alice" :name)))))

(test remove-node-property
  "Remove a property from a node"
  (let ((g (make-graph)))
    (add-node g "alice" :properties '((:name . "Alice") (:age . 30)))
    (remove-node-property g "alice" :age)
    (is-false (node-property g "alice" :age))
    (is (equal "Alice" (node-property g "alice" :name)))))

(test get-all-node-properties
  "Get all properties of a node"
  (let ((g (make-graph)))
    (add-node g "alice" :properties '((:name . "Alice") (:age . 30)))
    (let ((props (node-properties g "alice")))
      (is (= 2 (length props))))))

(test node-labels
  "Nodes can have labels (types)"
  (let ((g (make-graph)))
    (add-node g "alice" :labels '(:person :employee))
    (is-true (member :person (node-labels g "alice")))
    (is-true (member :employee (node-labels g "alice")))))

(test find-nodes-by-label
  "Find all nodes with a given label"
  (let ((g (make-graph)))
    (add-node g "alice" :labels '(:person))
    (add-node g "bob" :labels '(:person))
    (add-node g "acme" :labels '(:company))
    (let ((people (find-nodes g :label :person)))
      (is (= 2 (length people))))))

(test remove-node
  "Remove a node and all its edges"
  (let ((g (make-graph)))
    (add-node g "alice")
    (add-node g "bob")
    (add-edge g "alice" "bob" :knows)
    (remove-node g "alice")
    (is-false (get-node g "alice"))
    ;; Edge should also be removed
    (is (= 0 (length (get-edges g :from "alice"))))))

;; =============================================================================
;; Edge Operations
;; =============================================================================

(test create-edge
  "Create an edge between two nodes"
  (let ((g (make-graph)))
    (add-node g "alice")
    (add-node g "bob")
    (let ((e (add-edge g "alice" "bob" :knows)))
      (is-true e)
      (is (equal "alice" (edge-from e)))
      (is (equal "bob" (edge-to e)))
      (is (eq :knows (edge-type e))))))

(test create-edge-with-properties
  "Create an edge with properties"
  (let ((g (make-graph)))
    (add-node g "alice")
    (add-node g "bob")
    (add-edge g "alice" "bob" :knows :properties '((:since . 2020) (:weight . 0.9)))
    (let ((edges (get-edges g :from "alice" :type :knows)))
      (is (= 1 (length edges)))
      (is (= 2020 (edge-property (first edges) :since))))))

(test get-outgoing-edges
  "Get all outgoing edges from a node"
  (let ((g (make-graph)))
    (add-node g "alice")
    (add-node g "bob")
    (add-node g "charlie")
    (add-edge g "alice" "bob" :knows)
    (add-edge g "alice" "charlie" :knows)
    (add-edge g "bob" "charlie" :knows)
    (is (= 2 (length (get-edges g :from "alice"))))))

(test get-incoming-edges
  "Get all incoming edges to a node"
  (let ((g (make-graph)))
    (add-node g "alice")
    (add-node g "bob")
    (add-node g "charlie")
    (add-edge g "alice" "charlie" :knows)
    (add-edge g "bob" "charlie" :knows)
    (is (= 2 (length (get-edges g :to "charlie"))))))

(test get-edges-by-type
  "Get edges filtered by type"
  (let ((g (make-graph)))
    (add-node g "alice")
    (add-node g "bob")
    (add-edge g "alice" "bob" :knows)
    (add-edge g "alice" "bob" :likes)
    (is (= 1 (length (get-edges g :from "alice" :type :knows))))
    (is (= 1 (length (get-edges g :from "alice" :type :likes))))))

(test remove-edge
  "Remove a specific edge"
  (let ((g (make-graph)))
    (add-node g "alice")
    (add-node g "bob")
    (add-edge g "alice" "bob" :knows)
    (add-edge g "alice" "bob" :likes)
    (remove-edge g "alice" "bob" :knows)
    (is (= 0 (length (get-edges g :from "alice" :type :knows))))
    (is (= 1 (length (get-edges g :from "alice" :type :likes))))))

;; =============================================================================
;; Neighbors
;; =============================================================================

(test outgoing-neighbors
  "Get nodes connected by outgoing edges"
  (let ((g (make-graph)))
    (add-node g "alice")
    (add-node g "bob")
    (add-node g "charlie")
    (add-edge g "alice" "bob" :knows)
    (add-edge g "alice" "charlie" :knows)
    (is (= 2 (length (neighbors g "alice" :direction :out))))))

(test incoming-neighbors
  "Get nodes connected by incoming edges"
  (let ((g (make-graph)))
    (add-node g "alice")
    (add-node g "bob")
    (add-node g "charlie")
    (add-edge g "bob" "alice" :knows)
    (add-edge g "charlie" "alice" :knows)
    (is (= 2 (length (neighbors g "alice" :direction :in))))))

(test bidirectional-neighbors
  "Get all neighbors regardless of direction"
  (let ((g (make-graph)))
    (add-node g "alice")
    (add-node g "bob")
    (add-node g "charlie")
    (add-edge g "alice" "bob" :knows)
    (add-edge g "charlie" "alice" :knows)
    (is (= 2 (length (neighbors g "alice" :direction :both))))))
