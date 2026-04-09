# Property Graph

Higher-level API for labeled, typed nodes and edges. Built on top of the triple store using reserved predicates (`:ariadne/type`, `:ariadne/prop/*`, etc.).

[← Back to API Reference](../api-reference.md)

---

## add-node

```lisp
(add-node g id &key properties labels) → node
```

Create a node with optional labels and properties. If the node already exists, updates its labels and properties.

```lisp
;; Simple node
(add-node g "alice")

;; With labels and properties
(add-node g "alice"
          :labels '(:person :employee)
          :properties '((:name . "Alice") (:age . 30)))

;; Multiple labels for classification
(add-node g "sensor-1"
          :labels '(:device :sensor :iot)
          :properties '((:location . "building-a") (:type . "temperature")))
```

---

## get-node

```lisp
(get-node g id) → node-or-nil
```

Retrieve a node by ID. Returns NIL if the node doesn't exist.

```lisp
(add-node g "alice" :labels '(:person))
(get-node g "alice")    ; => #<NODE alice>
(get-node g "unknown")  ; => NIL
```

---

## remove-node

```lisp
(remove-node g id) → boolean
```

Remove a node and all its connected edges, labels, and properties.

```lisp
(add-node g "alice" :labels '(:person) :properties '((:name . "Alice")))
(add-edge g "alice" "bob" :knows)
(remove-node g "alice")
(get-node g "alice")                          ; => NIL
(get-edges g :from "alice" :type :knows)      ; => NIL
```

---

## node-id

```lisp
(node-id node) → value
```

Return the ID of a node object.

```lisp
(let ((n (add-node g "alice")))
  (node-id n))  ; => "alice"
```

---

## node-property

```lisp
(node-property g id prop) → value-or-nil
```

Get a single property value from a node.

```lisp
(add-node g "alice" :properties '((:name . "Alice") (:age . 30)))
(node-property g "alice" :name)  ; => "Alice"
(node-property g "alice" :age)   ; => 30
(node-property g "alice" :email) ; => NIL
```

---

## set-node-property

```lisp
(set-node-property g id prop value) → value
```

Set or update a property on a node.

```lisp
(add-node g "alice")
(set-node-property g "alice" :email "alice@example.com")
(node-property g "alice" :email)  ; => "alice@example.com"

;; Overwrite existing
(set-node-property g "alice" :email "new@example.com")
(node-property g "alice" :email)  ; => "new@example.com"
```

---

## remove-node-property

```lisp
(remove-node-property g id prop) → boolean
```

Remove a property from a node.

```lisp
(add-node g "alice" :properties '((:name . "Alice") (:age . 30)))
(remove-node-property g "alice" :age)
(node-property g "alice" :age)   ; => NIL
(node-property g "alice" :name)  ; => "Alice" (unchanged)
```

---

## node-properties

```lisp
(node-properties g id) → alist
```

Return all properties of a node as an association list.

```lisp
(add-node g "alice" :properties '((:name . "Alice") (:age . 30)))
(node-properties g "alice")
;; => ((:NAME . "Alice") (:AGE . 30))
```

---

## node-labels

```lisp
(node-labels g id) → list
```

Return all labels of a node.

```lisp
(add-node g "alice" :labels '(:person :employee))
(node-labels g "alice")  ; => (:PERSON :EMPLOYEE)
```

---

## find-nodes

```lisp
(find-nodes g &key label) → list-of-nodes
```

Find all nodes matching the given label.

```lisp
(add-node g "alice" :labels '(:person))
(add-node g "bob" :labels '(:person))
(add-node g "acme" :labels '(:company))
(find-nodes g :label :person)
;; => (#<NODE alice> #<NODE bob>)
```

---

## add-edge

```lisp
(add-edge g from to type &key properties) → edge
```

Create a typed, directed edge between two nodes. Optionally attach properties.

```lisp
;; Simple edge
(add-edge g "alice" "bob" :knows)

;; With properties
(add-edge g "alice" "bob" :knows
          :properties '((:since . 2020) (:weight . 0.9)))
```

---

## remove-edge

```lisp
(remove-edge g from to type) → boolean
```

Remove a specific edge.

```lisp
(add-edge g "alice" "bob" :knows)
(remove-edge g "alice" "bob" :knows)
(get-edges g :from "alice" :type :knows)  ; => NIL
```

---

## get-edges

```lisp
(get-edges g &key from to type) → list-of-edges
```

Query edges by source, target, and/or type.

```lisp
(add-edge g "alice" "bob" :knows)
(add-edge g "alice" "charlie" :knows)
(add-edge g "alice" "acme" :works-at)

(get-edges g :from "alice")                ; => 3 edges
(get-edges g :from "alice" :type :knows)   ; => 2 edges
(get-edges g :to "bob")                    ; => 1 edge
```

---

## edge-from

```lisp
(edge-from edge) → value
```

Return the source node ID of an edge.

---

## edge-to

```lisp
(edge-to edge) → value
```

Return the target node ID of an edge.

---

## edge-type

```lisp
(edge-type edge) → keyword
```

Return the type of an edge.

```lisp
(let ((e (add-edge g "alice" "bob" :knows)))
  (edge-from e)  ; => "alice"
  (edge-to e)    ; => "bob"
  (edge-type e)) ; => :KNOWS
```

---

## edge-property

```lisp
(edge-property g edge prop) → value-or-nil
```

Get a property value from an edge.

```lisp
(add-edge g "alice" "bob" :knows :properties '((:since . 2020)))
(let ((e (first (get-edges g :from "alice" :type :knows))))
  (edge-property g e :since))  ; => 2020
```

---

## neighbors

```lisp
(neighbors g id &key direction type) → list
```

Get neighboring nodes. Direction can be `:out`, `:in`, or `:both`.

```lisp
(add-edge g "alice" "bob" :knows)
(add-edge g "charlie" "alice" :knows)

(neighbors g "alice" :direction :out)   ; => ("bob")
(neighbors g "alice" :direction :in)    ; => ("charlie")
(neighbors g "alice" :direction :both)  ; => ("bob" "charlie")
```
