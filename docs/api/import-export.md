# Import / Export

Functions for reading and writing RDF serialization formats.

[← Back to API Reference](../api-reference.md)

---

## import-ntriples

```lisp
(import-ntriples g string) → integer
```

Import N-Triples from a string. Returns the number of triples imported. Supports URIs, typed literals, language-tagged strings, and blank nodes.

```lisp
(import-ntriples g
  "<http://example.org/alice> <http://xmlns.com/foaf/0.1/knows> <http://example.org/bob> .
   <http://example.org/alice> <http://xmlns.com/foaf/0.1/age> \"30\"^^<http://www.w3.org/2001/XMLSchema#integer> .")

;; Typed literals are parsed: "30"^^xsd:integer becomes CL integer 30
(triple-object (first (get-triples g :predicate "http://xmlns.com/foaf/0.1/age")))
;; => 30
```

---

## import-ntriples-file

```lisp
(import-ntriples-file g path) → integer
```

Import N-Triples from a file path.

```lisp
(import-ntriples-file g #p"/data/dbpedia-cities.nt")
```

---

## export-ntriples

```lisp
(export-ntriples g) → string
```

Export all triples as an N-Triples format string.

```lisp
(add-triple g "http://example.org/alice" "http://example.org/knows" "http://example.org/bob")
(export-ntriples g)
;; => "<http://example.org/alice> <http://example.org/knows> <http://example.org/bob> .\n"
```

---

## import-turtle

```lisp
(import-turtle g string) → integer
```

Import Turtle format from a string. Supports `@prefix`, semicolons, commas, `a` shorthand, quoted strings, numbers, blank nodes, and long literals. W3C conformant (213/213 positive tests).

```lisp
(import-turtle g
  "@prefix foaf: <http://xmlns.com/foaf/0.1/> .
   @prefix ex: <http://example.org/> .
   ex:alice foaf:name \"Alice\" ;
            foaf:knows ex:bob , ex:charlie .")
;; Imports 3 triples
```

---

## export-turtle

```lisp
(export-turtle g) → string
```

Export all triples as Turtle format with automatic prefix detection, semicolon/comma shorthand, and `a` for rdf:type.

```lisp
(export-turtle g)
;; => "@prefix foaf: <http://xmlns.com/foaf/0.1/> .
;;     ex:alice foaf:name \"Alice\" ;
;;             foaf:knows ex:bob , ex:charlie ."
```

---

## import-nquads

```lisp
(import-nquads g string) → integer
```

Import N-Quads from a string. Like N-Triples but with an optional fourth element specifying the graph name.

```lisp
(import-nquads g
  "<http://ex.org/a> <http://ex.org/knows> <http://ex.org/b> <http://ex.org/g1> .")
(named-graphs g)  ; => ("http://ex.org/g1")
```

---

## export-nquads

```lisp
(export-nquads g) → string
```

Export all triples as N-Quads format. Triples with graph names include the fourth element; triples without graph names are exported as plain N-Triples lines.

```lisp
(add-quad g "a" "b" "c" "http://example.org/g1")
(export-nquads g)
;; => "<a> <b> <c> <http://example.org/g1> .\n"
```

---

## stream-import-ntriples

```lisp
(stream-import-ntriples g path) → integer
```

Stream N-Triples from a file line by line. Memory-efficient for large files that don't fit as strings.

```lisp
(stream-import-ntriples g #p"/data/large-dataset.nt")
;; Processes millions of lines without loading entire file into memory
```

---

## stream-import-nquads

```lisp
(stream-import-nquads g path) → integer
```

Stream N-Quads from a file line by line. Preserves graph names. Tested at 3.6M triples (drugbank, 34 seconds).

```lisp
(stream-import-nquads g #p"/data/drugbank-full.nq")
```
