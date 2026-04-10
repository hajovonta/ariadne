# Schema Validation

Define schemas with property types and cardinality constraints, validate graph instances, and find similar entities.

## `define-schema`

```lisp
(define-schema graph class-uri &key properties required)
```

Define a schema for a class. Each property spec is `(predicate type &key min max)`.

### Example

```lisp
(define-schema g "http://ex.org/Person"
  :properties '(("http://ex.org/name" :string :min 1 :max 1)
                ("http://ex.org/age" :number :min 0 :max 1)
                ("http://ex.org/knows" :uri)))
```

## `graph-schema`

```lisp
(graph-schema graph)
```

Return the schema definitions for the graph.

## `validate-graph`

```lisp
(validate-graph graph)
```

Check all typed instances against their class schemas. Returns a list of violation plists with `:entity`, `:class`, `:property`, and `:message`.

### Example

```lisp
(let ((violations (validate-graph g)))
  (dolist (v violations)
    (format t "~A: ~A~%" (getf v :entity) (getf v :message))))
```

## `find-similar-entities`

```lisp
(find-similar-entities graph entity &key threshold)
```

Find entities similar to ENTITY using Jaccard bigram similarity on their property values. Returns entities with similarity above THRESHOLD (default 0.5).

### Example

```lisp
(find-similar-entities g "http://ex.org/alice" :threshold 0.3)
;; => (("http://ex.org/alicia" . 0.75) ("http://ex.org/bob" . 0.1))
```
