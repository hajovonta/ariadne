# Export Formats

Additional export formats beyond N-Triples and Turtle.

## `export-json-ld`

```lisp
(export-json-ld graph)
```

Export graph as a JSON-LD string. Groups triples by subject into JSON objects with `@id`.

### Example

```lisp
(let ((g (make-graph :name "jld")))
  (add-triple g "http://ex.org/alice" "http://ex.org/name" "Alice")
  (export-json-ld g))
;; => "[{\"@id\":\"http://ex.org/alice\",\"http://ex.org/name\":\"Alice\"}]"
```

## `export-cytoscape-json`

```lisp
(export-cytoscape-json graph &key predicates center depth)
```

Export graph as Cytoscape.js compatible JSON elements array. Optionally filter by predicates, center on a node, or limit traversal depth.

## `export-nquads`

```lisp
(export-nquads graph)
```

Export graph as N-Quads string, including named graph information.

## `import-rdf-xml`

```lisp
(import-rdf-xml graph string)
```

Import RDF/XML data from a string (regex-based parser, no XML library dependency).

## `import-json-ld`

```lisp
(import-json-ld graph string)
```

Import JSON-LD data from a string. Supports roundtrip with `export-json-ld`.
