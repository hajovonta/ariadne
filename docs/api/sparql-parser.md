# SPARQL String Parser

Execute standard SPARQL query strings directly.

[← Back to API Reference](../api-reference.md)

---

## sparql

```lisp
(sparql g query-string) → results
```

Parse a SPARQL query string, translate it to the internal DSL, and execute it. Supports SELECT, ASK, PREFIX, FILTER, DISTINCT, LIMIT, ORDER BY. Returns the same result types as `query`.

```lisp
;; Basic SELECT
(sparql g "SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }")
;; => (("Alice") ("Bob"))

;; With PREFIX declarations
(sparql g "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
           SELECT ?name ?age WHERE {
             ?person foaf:name ?name .
             ?person foaf:age ?age
           }")

;; ASK query
(sparql g "ASK { <http://example.org/alice> <http://example.org/knows> <http://example.org/bob> }")
;; => T

;; FILTER, DISTINCT, LIMIT, ORDER BY
(sparql g "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
           SELECT DISTINCT ?name WHERE {
             ?person foaf:name ?name .
             ?person foaf:age ?age .
             FILTER(?age > 30)
           } ORDER BY ?name LIMIT 10")
```
