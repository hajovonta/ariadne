# SHACL Validation

W3C Shapes Constraint Language validation — 95/95 core tests (100% conformance).

## `shacl-validate`

```lisp
(shacl-validate graph)
```

Validate graph G against all SHACL shapes defined within it. Shapes and data can coexist in the same graph. Returns a plist with `:conforms` (boolean) and `:results` (list of violation plists).

### Supported Features

**Targets:** `sh:targetClass` (with subclass traversal), `sh:targetNode`, `sh:targetSubjectsOf`, `sh:targetObjectsOf`, implicit target class

**Constraints:** `sh:minCount`, `sh:maxCount`, `sh:datatype`, `sh:class`, `sh:nodeKind`, `sh:pattern`, `sh:in`, `sh:hasValue`, `sh:minInclusive`, `sh:maxInclusive`, `sh:minExclusive`, `sh:maxExclusive`, `sh:minLength`, `sh:maxLength`, `sh:equals`, `sh:disjoint`, `sh:lessThan`, `sh:lessThanOrEquals`, `sh:uniqueLang`, `sh:languageIn`, `sh:closed`, `sh:qualifiedValueShape`, `sh:qualifiedValueShapesDisjoint`, `sh:deactivated`

**Logical:** `sh:not`, `sh:and`, `sh:or`, `sh:xone`

**Shapes:** `sh:NodeShape`, `sh:PropertyShape`, `sh:node` (nested shape reference), nested `sh:property`

**Paths:** `sh:inversePath`, `sh:alternativePath`, `sh:zeroOrMorePath`, `sh:oneOrMorePath`, `sh:zeroOrOnePath`, sequence paths

**Value types:** Numbers, strings, booleans, language-tagged strings, `xsd:dateTime` (via local-time), `xsd:byte`/`short`/`int` range validation

### Example

```lisp
(let ((g (make-graph :name "shacl-demo")))
  ;; Define shape
  (add-triple g "ex:PersonShape" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
              "http://www.w3.org/ns/shacl#NodeShape")
  (add-triple g "ex:PersonShape" "http://www.w3.org/ns/shacl#targetClass" "ex:Person")
  (add-triple g "ex:PersonShape" "http://www.w3.org/ns/shacl#property" "ex:NameProp")
  (add-triple g "ex:NameProp" "http://www.w3.org/ns/shacl#path" "ex:name")
  (add-triple g "ex:NameProp" "http://www.w3.org/ns/shacl#minCount" "1")
  ;; Data
  (add-triple g "ex:alice" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "ex:Person")
  (add-triple g "ex:alice" "ex:name" "Alice")
  (add-triple g "ex:bob" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "ex:Person")
  ;; bob has no name — violation
  (let ((report (shacl-validate g)))
    (format t "Conforms: ~A~%" (getf report :conforms))
    (dolist (r (getf report :results))
      (format t "  ~A: ~A~%" (getf r :focus-node) (getf r :result-message)))))
;; Conforms: NIL
;;   ex:bob: minCount 1 but found 0
```

### Validation Report

Each result in `:results` is a plist with:

| Key | Value |
|-----|-------|
| `:focus-node` | The node that failed validation |
| `:result-path` | The property path (or NIL for node constraints) |
| `:source-shape` | The shape that produced the violation |
| `:result-message` | Human-readable description |
| `:value` | The offending value (if applicable) |
| `:result-severity` | `sh:Violation` (default) |
