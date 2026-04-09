# Graph Analytics

Built-in graph algorithms operating on the triple store.

[← Back to API Reference](../api-reference.md)

---

## pagerank

```lisp
(pagerank g &key predicate iterations damping) → alist
```

Compute PageRank scores for all nodes connected by the given predicate. Returns an association list of (node . score) pairs sorted by score descending. Default: 20 iterations, 0.85 damping factor.

```lisp
;; Basic PageRank
(add-triple g "a" "links" "b")
(add-triple g "b" "links" "c")
(add-triple g "c" "links" "a")
(pagerank g :predicate "links")
;; => (("a" . 0.33) ("b" . 0.33) ("c" . 0.33))

;; Hub detection: node with many incoming links scores higher
(add-triple g "d" "links" "a")
(add-triple g "e" "links" "a")
(first (pagerank g :predicate "links"))
;; => ("a" . 0.42)  — highest score

;; Custom parameters
(pagerank g :predicate "cites" :iterations 50 :damping 0.9)
```

---

## connected-components

```lisp
(connected-components g &key predicate) → list-of-lists
```

Find connected components in the graph. Returns a list of lists, where each inner list contains the nodes in one component. Treats edges as undirected.

```lisp
(add-triple g "alice" "knows" "bob")
(add-triple g "bob" "knows" "charlie")
(add-triple g "dave" "knows" "eve")
(connected-components g :predicate "knows")
;; => (("alice" "bob" "charlie") ("dave" "eve"))

;; Single isolated node won't appear unless it has at least one edge
```

---

## degree-centrality

```lisp
(degree-centrality g &key predicate) → alist
```

Compute degree centrality (number of connections) for each node. Returns an association list sorted by degree descending. Counts both incoming and outgoing edges.

```lisp
(add-triple g "alice" "knows" "bob")
(add-triple g "alice" "knows" "charlie")
(add-triple g "bob" "knows" "charlie")
(degree-centrality g :predicate "knows")
;; => (("alice" . 2) ("charlie" . 2) ("bob" . 2))

;; Find the most connected node
(first (degree-centrality g :predicate "knows"))
```

---

## clustering-coefficient

```lisp
(clustering-coefficient g &key predicate) → alist
```

Compute the local clustering coefficient for each node. Measures how close a node's neighbors are to forming a complete graph. Values range from 0.0 (no clustering) to 1.0 (fully connected neighborhood).

```lisp
;; Triangle: all neighbors connected
(add-triple g "a" "knows" "b")
(add-triple g "b" "knows" "c")
(add-triple g "a" "knows" "c")
(clustering-coefficient g :predicate "knows")
;; => (("a" . 1.0) ("b" . 1.0) ("c" . 1.0))

;; Star: hub connected to leaves, leaves not connected to each other
(add-triple g "hub" "links" "l1")
(add-triple g "hub" "links" "l2")
(add-triple g "hub" "links" "l3")
(assoc "hub" (clustering-coefficient g :predicate "links") :test #'equal)
;; => ("hub" . 0.0)
```
