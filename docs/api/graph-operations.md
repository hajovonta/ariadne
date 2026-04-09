# Graph Operations

Functions for merging, comparing, and copying graphs.

[← Back to API Reference](../api-reference.md)

---

## merge-graphs

```lisp
(merge-graphs g1 g2) → graph
```

Create a new graph containing all triples from both `g1` and `g2`. Duplicate triples are automatically deduplicated. The new graph inherits the name of `g1`.

```lisp
(let ((g1 (make-graph :name "social"))
      (g2 (make-graph)))
  (add-triple g1 "alice" "knows" "bob")
  (add-triple g2 "bob" "knows" "charlie")
  (add-triple g2 "alice" "knows" "bob")  ; duplicate
  (let ((merged (merge-graphs g1 g2)))
    (triple-count merged)     ; => 2 (deduplicated)
    (graph-name merged)))     ; => "social"
```

---

## merge-graphs-into

```lisp
(merge-graphs-into target source) → target
```

Add all triples from `source` into `target` in place. Returns the modified `target` graph.

```lisp
(let ((main (make-graph))
      (extra (make-graph)))
  (add-triple main "alice" "knows" "bob")
  (add-triple extra "bob" "knows" "charlie")
  (merge-graphs-into main extra)
  (triple-count main))  ; => 2

;; Useful for combining multiple data sources
(dolist (file (directory #p"data/*.ttl"))
  (let ((tmp (make-graph)))
    (import-turtle tmp (uiop:read-file-string file))
    (merge-graphs-into main tmp)))
```

---

## diff-graphs

```lisp
(diff-graphs g1 g2) → list-of-triples
```

Return triples that exist in `g1` but not in `g2`. Useful for finding what was added or changed between two versions.

```lisp
(let ((before (make-graph))
      (after (make-graph)))
  (add-triple before "alice" "knows" "bob")
  (add-triple after "alice" "knows" "bob")
  (add-triple after "alice" "knows" "charlie")
  (diff-graphs after before))
;; => (#<TRIPLE alice knows charlie>)

;; Symmetric diff: changes in both directions
(let ((added (diff-graphs after before))
      (removed (diff-graphs before after)))
  (format t "Added: ~A, Removed: ~A~%" (length added) (length removed)))
```

---

## copy-graph

```lisp
(copy-graph g) → graph
```

Create an independent deep copy of a graph. Changes to the original do not affect the copy and vice versa.

```lisp
(let* ((g1 (make-graph :name "original"))
       (_ (add-triple g1 "alice" "knows" "bob"))
       (g2 (copy-graph g1)))
  (add-triple g1 "bob" "knows" "charlie")
  (triple-count g1)   ; => 2
  (triple-count g2)   ; => 1 (independent)
  (graph-name g2))    ; => "original" (preserved)

;; Useful for creating snapshots before experiments
(let ((snapshot (copy-graph g)))
  ;; ... try things ...
  ;; If unhappy, still have snapshot
  )
```
