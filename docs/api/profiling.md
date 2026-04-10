# Profiling & Statistics

Query timing, graph statistics, and memory usage reporting.

## `profile-query`

```lisp
(profile-query graph expr)
```

Execute a query and return a profiling report string with timing and result count.

### Example

```lisp
(profile-query g '(select (?s) (where (?s "http://ex.org/type" "Person"))))
;; => "Query time: 0.0012 seconds, 42 results"
```

## `graph-statistics`

```lisp
(graph-statistics graph)
```

Return a plist of graph statistics: `:name`, `:triples`, `:subjects`, `:predicates`, `:objects`, `:spo-index-size`, `:interned-strings`.

## `graph-memory-usage`

```lisp
(graph-memory-usage graph)
```

Return a plist estimating memory usage: `:triple-count`, `:index-bytes`, `:intern-count`.

### Example

```lisp
(graph-memory-usage g)
;; => (:TRIPLE-COUNT 1000 :INDEX-BYTES 45632 :INTERN-COUNT 2500)
```
