# Full-Text Search

Inverted index over string literals for keyword search.

## `build-text-index`

```lisp
(build-text-index graph &key fields)
```

Build an inverted index over string values. FIELDS can be `:objects` (default) or `:all` (subjects + objects). Must be called before `text-search`.

## `text-search`

```lisp
(text-search graph query)
```

Search for triples matching all words in QUERY (case-insensitive AND matching). Returns a list of matching triples.

### Example

```lisp
(let ((g (make-graph :name "docs")))
  (add-triple g "http://ex.org/cl" "http://ex.org/desc" "Common Lisp programming language")
  (add-triple g "http://ex.org/py" "http://ex.org/desc" "Python programming language")
  (build-text-index g)
  (text-search g "common lisp"))
;; => (#<TRIPLE "http://ex.org/cl" ...>)

(text-search g "programming")
;; => (#<TRIPLE ...> #<TRIPLE ...>)  ; both match
```

## `graph-text-index`

```lisp
(graph-text-index graph)
```

Return the text index hash table, or NIL if not built yet.
