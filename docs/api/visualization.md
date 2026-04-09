# Visualization & REPL

Functions for rendering graphs and formatting output.

[← Back to API Reference](../api-reference.md)

---

## export-dot

```lisp
(export-dot g &key predicates center depth file) → string
```

Export the graph as a DOT/Graphviz format string. Optionally filter by predicates or extract a subgraph around a center node.

```lisp
;; Full graph as DOT string
(export-dot g)
;; => "digraph ariadne { \"alice\" -> \"bob\" [label=\"knows\"]; ... }"

;; Only specific predicates
(export-dot g :predicates '("knows" "likes"))

;; Subgraph: 2 hops around alice
(export-dot g :center "alice" :depth 2)

;; Write to file
(export-dot g :file #p"graph.dot")
```

---

## visualize-graph

```lisp
(visualize-graph g &key file engine predicates center depth open) → pathname
```

Render the graph to PNG, SVG, or PDF using Graphviz. Requires `dot` (Graphviz) to be installed. The output format is detected from the file extension.

Available engines: `:dot` (hierarchical), `:neato` (force-directed), `:fdp` (spring), `:circo` (circular), `:twopi` (radial), `:sfdp` (scalable force-directed).

```lisp
;; Basic PNG
(visualize-graph g :file #p"graph.png")

;; Force-directed layout for social networks
(visualize-graph g :file #p"social.svg" :engine :neato
                    :predicates '("knows"))

;; Radial layout centered on a node
(visualize-graph g :file #p"orbits.png" :engine :twopi
                    :predicates '("orbits" "hasMoon")
                    :center "Sol" :depth 3)

;; Open in system viewer
(visualize-graph g :file #p"graph.png" :open t)
```

---

## describe-graph

```lisp
(describe-graph g) → string
```

Return a human-readable summary of the graph: name, triple count, subject/predicate/object counts, and top predicates by frequency.

```lisp
(format t "~A" (describe-graph g))
;; Perihelion Knowledge Graph
;; 1551 triples, 245 subjects, 28 predicates, 892 unique objects
;; Top predicates:
;;   rdf:type (332)
;;   rdfs:label (280)
;;   p:definedIn (120)

;; Useful after loading a new dataset
(import-turtle g (uiop:read-file-string #p"data.ttl"))
(format t "~A" (describe-graph g))
```

---

## format-results

```lisp
(format-results rows headers) → string
```

Format query results as an aligned ASCII table. Useful for REPL exploration.

```lisp
(format-results
  '(("alice" 30 "engineer") ("bob" 25 "designer"))
  '("name" "age" "role"))
;; +---------+-----+----------+
;; | name    | age | role     |
;; +---------+-----+----------+
;; | alice   |  30 | engineer |
;; | bob     |  25 | designer |
;; +---------+-----+----------+

;; Combine with query
(let ((results (query g '(select (?name ?age)
                          (where (?p "name" ?name)
                                 (?p "age" ?age))))))
  (format-results results '("name" "age")))
```
