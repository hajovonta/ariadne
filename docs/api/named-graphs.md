# Named Graphs

Partition triples into named contexts for provenance tracking and dataset management.

[← Back to API Reference](../api-reference.md)

---

## add-quad

```lisp
(add-quad g subject predicate object graph-name) → triple
```

Add a triple associated with a named graph. The triple is stored in the main index and additionally tagged with the graph name.

```lisp
;; Tag triples with their source
(add-quad g "alice" "knows" "bob" "http://example.org/social")
(add-quad g "alice" "age" 30 "http://example.org/personal")

;; Same triple can exist in multiple graphs
(add-quad g "alice" "knows" "bob" "http://example.org/backup")
```

---

## get-quads

```lisp
(get-quads g &key subject predicate object graph) → list
```

Query triples filtered by graph name. Supports all the same constraints as `get-triples` plus a `:graph` filter.

```lisp
;; All triples in a specific graph
(get-quads g :graph "http://example.org/social")

;; Combined filters
(get-quads g :subject "alice" :graph "http://example.org/social")

;; Across all graphs (same as get-triples)
(get-quads g :subject "alice")
```

---

## named-graphs

```lisp
(named-graphs g) → list
```

Return a list of all named graph URIs in the graph.

```lisp
(add-quad g "a" "b" "c" "http://example.org/g1")
(add-quad g "d" "e" "f" "http://example.org/g2")
(named-graphs g)
;; => ("http://example.org/g1" "http://example.org/g2")
```
