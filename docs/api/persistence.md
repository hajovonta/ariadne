# Persistence

Save and load graphs to disk using CL's print/read for full type preservation.

[← Back to API Reference](../api-reference.md)

---

## save-graph

```lisp
(save-graph g path) → pathname
```

Save the entire graph to a file. Uses CL's `print` for serialization — strings, numbers, symbols, and keywords all survive the roundtrip. The graph name is preserved.

```lisp
;; Save to file
(save-graph g #p"~/data/social.ariadne")

;; Overwrite existing
(save-graph g #p"~/data/social.ariadne")

;; Use with transactions for safe saves
(with-transaction (g)
  (add-triple g "new" "data" "here")
  (save-graph g #p"~/data/social.ariadne"))
```

---

## load-graph

```lisp
(load-graph path) → graph
```

Load a graph from a file previously saved with `save-graph`. Returns a new graph object with all triples, graph name, and metadata restored.

```lisp
;; Load and use
(defparameter *g* (load-graph #p"~/data/social.ariadne"))
(graph-name *g*)     ; => "social"
(triple-count *g*)   ; => 1551

;; Roundtrip verification
(save-graph g #p"/tmp/test.ariadne")
(let ((g2 (load-graph #p"/tmp/test.ariadne")))
  (= (triple-count g) (triple-count g2)))  ; => T
```
