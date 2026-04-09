# Traversal

Gremlin-style imperative graph walking with chainable steps.

[← Back to API Reference](../api-reference.md)

---

## traverse

```lisp
(traverse g start &rest steps) → list
```

Walk the graph from a starting node through a sequence of steps. Each step takes the current set of nodes and produces a new set. Steps: `(out "pred")`, `(in "pred")`, `(both "pred")`, `(has "pred" value)`, `(values "pred")`.

```lisp
;; Single hop
(traverse g "alice" '(out "knows"))
;; => ("bob" "charlie")

;; Two hops: friend of friend
(traverse g "alice" '(out "knows") '(out "knows"))
;; => ("dave")

;; Filter by property
(traverse g "alice"
          '(out "knows")
          '(has "age" (> 30)))
;; => ("charlie")

;; Extract values
(traverse g "alice"
          '(out "knows")
          '(values "name"))
;; => ("Bob" "Charlie")
```

---

## traverse-with-path

```lisp
(traverse-with-path g start &rest steps) → list-of-paths
```

Like `traverse`, but returns full paths (lists of nodes visited) instead of just endpoints.

```lisp
(traverse-with-path g "alice" '(out "knows") '(out "knows"))
;; => (("alice" "bob" "dave") ("alice" "charlie" "eve"))

;; Useful for understanding how nodes are connected
(traverse-with-path g "a" '(out "link") '(out "link") '(out "link"))
;; => (("a" "b" "c" "d"))
```

---

## traverse-depth

```lisp
(traverse-depth g start predicate &key max-depth) → list
```

Depth-limited traversal following a single predicate. Cycle-safe — each node visited at most once. Returns all reachable nodes within the depth limit.

```lisp
;; All nodes reachable within 2 hops
(traverse-depth g "alice" "knows" :max-depth 2)
;; => ("bob" "charlie" "dave")

;; Unlimited depth (finds all reachable nodes)
(traverse-depth g "alice" "knows")
;; => ("bob" "charlie" "dave" "eve")

;; Handles cycles safely
(add-triple g "dave" "knows" "alice")
(traverse-depth g "alice" "knows" :max-depth 10)
;; => ("bob" "charlie" "dave" "eve")  — no infinite loop
```

---

## shortest-path

```lisp
(shortest-path g from to &key edge-type) → list-or-nil
```

Find the shortest path between two nodes using BFS. Returns the path as a list of nodes, or NIL if no path exists.

```lisp
(shortest-path g "alice" "dave" :edge-type "knows")
;; => ("alice" "bob" "dave")

;; No path
(shortest-path g "alice" "isolated" :edge-type "knows")
;; => NIL

;; Direct connection
(shortest-path g "alice" "bob" :edge-type "knows")
;; => ("alice" "bob")
```
