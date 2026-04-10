# SPARQL UPDATE

Execute SPARQL UPDATE operations via string parsing.

## `sparql-update`

```lisp
(sparql-update graph update-string)
```

Execute a SPARQL UPDATE string. Supports INSERT DATA, DELETE DATA, and DELETE WHERE. Returns the number of triples affected.

### INSERT DATA

```lisp
(sparql-update g "PREFIX ex: <http://ex.org/>
INSERT DATA { ex:alice ex:knows ex:bob . ex:bob ex:age \"25\" }")
;; => 2
```

### DELETE DATA

```lisp
(sparql-update g "DELETE DATA { <http://ex.org/alice> <http://ex.org/knows> <http://ex.org/bob> }")
;; => 1
```

### DELETE WHERE

Bulk delete by pattern matching with variables:

```lisp
;; Delete all triples about alice
(sparql-update g "DELETE WHERE { <http://ex.org/alice> ?p ?o }")

;; Delete everything
(sparql-update g "DELETE WHERE { ?s ?p ?o }")
```
