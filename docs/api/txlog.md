# Transaction Log

Append-only transaction log for incremental persistence and crash recovery.

## `start-txlog`

```lisp
(start-txlog graph path)
```

Start logging all add/remove operations to the file at PATH. Thread-safe.

## `stop-txlog`

```lisp
(stop-txlog graph)
```

Stop logging and close the log file.

## `replay-txlog`

```lisp
(replay-txlog graph path)
```

Replay a transaction log file to reconstruct graph state. Use after loading a base snapshot to apply incremental changes.

### Example

```lisp
(let ((g (make-graph :name "logged")))
  (start-txlog g "/tmp/ariadne.txlog")
  (add-triple g "alice" "knows" "bob")
  (stop-txlog g)

  ;; Later: recover
  (let ((g2 (make-graph :name "recovered")))
    (replay-txlog g2 "/tmp/ariadne.txlog")
    (triple-count g2)))
;; => 1
```
