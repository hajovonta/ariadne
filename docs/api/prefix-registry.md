# Prefix Registry

Register short prefixes to avoid writing full URIs in queries and triple operations.

## `register-prefix`

```lisp
(register-prefix graph prefix uri)
```

Register a prefix for use in `add-triple`, `get-triples`, `has-triple-p`, `match-pattern`, and related functions.

### Example

```lisp
(register-prefix g "ex" "http://example.org/")
(add-triple g "ex:alice" "ex:knows" "ex:bob")
;; Stored as full URIs internally
(has-triple-p g "http://example.org/alice" "http://example.org/knows" "http://example.org/bob")
;; => T
```

## `register-common-prefixes`

```lisp
(register-common-prefixes graph)
```

Register standard prefixes: `rdf`, `rdfs`, `owl`, `xsd`, `foaf`, `dc`, `skos`, `schema`.

## `graph-prefixes`

```lisp
(graph-prefixes graph)
```

Return an alist of registered `(prefix . uri)` pairs.
