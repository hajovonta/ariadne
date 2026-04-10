# Graph Versioning

Named snapshots with temporal queries against historical states.

## `graph-checkpoint`

```lisp
(graph-checkpoint graph name)
```

Save the current graph state as a named version.

## `graph-restore`

```lisp
(graph-restore graph name)
```

Restore the graph to a previously checkpointed version. Clears current state and replays the snapshot.

## `graph-versions`

```lisp
(graph-versions graph)
```

Return a list of version plists, each with `:name` and `:timestamp`.

## `query-at-version`

```lisp
(query-at-version graph version-name expr)
```

Execute a query expression against a historical version without modifying the current graph.

### Example

```lisp
(let ((g (make-graph :name "temporal")))
  (add-triple g "alice" "age" "30")
  (graph-checkpoint g "v1")

  (add-triple g "bob" "age" "25")
  (graph-checkpoint g "v2")

  ;; Query the past
  (query-at-version g "v1" '(select (?s ?o) (where (?s "age" ?o))))
  ;; => (("alice" "30"))

  ;; Current state has both
  (triple-count g)
  ;; => 2

  ;; Rollback
  (graph-restore g "v1")
  (triple-count g))
  ;; => 1
```
