# OWL/RDFS Reasoning

Automatic entailment using standard OWL and RDFS vocabulary.

## `apply-owl-rules`

```lisp
(apply-owl-rules graph)
```

Apply OWL/RDFS reasoning rules to the graph until no new triples are inferred. Supports:

- **rdfs:subClassOf** — transitive class hierarchy
- **rdfs:subPropertyOf** — property hierarchy
- **rdfs:domain / rdfs:range** — type inference from property usage
- **owl:inverseOf** — generate inverse triples
- **owl:TransitiveProperty** — transitive closure
- **owl:SymmetricProperty** — symmetric triples
- **owl:sameAs** — identity propagation

Returns the number of new triples added.

### Examples

```lisp
(let ((g (make-graph :name "ontology")))
  ;; Define class hierarchy
  (add-triple g "http://ex.org/Dog" "http://www.w3.org/2000/01/rdf-schema#subClassOf" "http://ex.org/Animal")
  (add-triple g "http://ex.org/fido" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "http://ex.org/Dog")
  (apply-owl-rules g)
  ;; fido is now also typed as Animal
  (has-triple-p g "http://ex.org/fido" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "http://ex.org/Animal"))
;; => T
```

```lisp
;; Inverse properties
(add-triple g "http://ex.org/parent" "http://www.w3.org/2002/07/owl#inverseOf" "http://ex.org/child")
(add-triple g "http://ex.org/alice" "http://ex.org/parent" "http://ex.org/bob")
(apply-owl-rules g)
(has-triple-p g "http://ex.org/bob" "http://ex.org/child" "http://ex.org/alice")
;; => T
```
