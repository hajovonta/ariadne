# Transactions

Snapshot-based transactions with automatic rollback on error.

[← Back to API Reference](../api-reference.md)

---

## with-transaction

```lisp
(with-transaction (graph) &body body) → results
```

Macro that captures a snapshot before executing `body`. If any error occurs, the graph is rolled back to the snapshot state. On success, changes are committed.

```lisp
;; Success: both triples committed
(with-transaction (g)
  (add-triple g "alice" "knows" "bob")
  (add-triple g "bob" "knows" "charlie"))
(triple-count g)  ; => 2

;; Error: graph unchanged
(handler-case
    (with-transaction (g)
      (add-triple g "alice" "knows" "dave")
      (error "abort!"))
  (error () nil))
(has-triple-p g "alice" "knows" "dave")  ; => NIL

;; Useful for batch imports that might fail
(with-transaction (g)
  (import-turtle g (uiop:read-file-string #p"data.ttl")))
```

---

## begin-transaction

```lisp
(begin-transaction g) → transaction
```

Manually start a transaction by capturing a snapshot of the current graph state. Use with `rollback-transaction` for explicit control.

```lisp
(let ((tx (begin-transaction g)))
  (add-triple g "alice" "knows" "bob")
  ;; Decide to undo
  (rollback-transaction tx))
(has-triple-p g "alice" "knows" "bob")  ; => NIL
```

---

## rollback-transaction

```lisp
(rollback-transaction transaction) → nil
```

Restore the graph to the state captured when the transaction was started. All changes made since `begin-transaction` are discarded.

```lisp
(add-triple g "existing" "triple" "here")
(let ((tx (begin-transaction g)))
  (add-triple g "new" "triple" "1")
  (add-triple g "new" "triple" "2")
  (triple-count g)           ; => 3
  (rollback-transaction tx)
  (triple-count g))          ; => 1
```
