# Graph

Core graph creation and inspection functions.

[← Back to API Reference](../api-reference.md)

---

## make-graph

```lisp
(make-graph &key name) → graph
```

Create a new empty graph. The optional `name` is stored as metadata and used in DOT export and `describe-graph` output.

```lisp
;; Anonymous graph
(defparameter *g* (make-graph))

;; Named graph
(defparameter *social* (make-graph :name "social-network"))

;; Name appears in exports
(export-dot *social*)  ; => "digraph social_network { ... }"
```

---

## graphp

```lisp
(graphp x) → boolean
```

Test whether `x` is a graph object.

```lisp
(graphp (make-graph))   ; => T
(graphp "not a graph")  ; => NIL
(graphp 42)             ; => NIL
```

---

## graph-name

```lisp
(graph-name graph) → string-or-nil
```

Return the name of the graph, or NIL if unnamed.

```lisp
(graph-name (make-graph :name "test"))  ; => "test"
(graph-name (make-graph))               ; => NIL
```

---

## triple-count

```lisp
(triple-count graph) → integer
```

Return the number of triples currently stored in the graph. Duplicates are never counted (they are deduplicated on insert).

```lisp
(let ((g (make-graph)))
  (triple-count g)                        ; => 0
  (add-triple g "a" "knows" "b")
  (triple-count g)                        ; => 1
  (add-triple g "a" "knows" "b")          ; duplicate, ignored
  (triple-count g))                       ; => 1
```

---

## clear-graph

```lisp
(clear-graph graph) → nil
```

Remove all triples and reset all indexes. Named graph associations and triggers are preserved.

```lisp
(let ((g (make-graph)))
  (add-triple g "a" "knows" "b")
  (add-triple g "b" "knows" "c")
  (triple-count g)    ; => 2
  (clear-graph g)
  (triple-count g))   ; => 0
```
