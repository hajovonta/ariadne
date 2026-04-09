# Triples

Functions for adding, removing, querying, and inspecting triples.

[← Back to API Reference](../api-reference.md)

---

## add-triple

```lisp
(add-triple g subject predicate object) → triple
```

Add a triple to the graph. Returns the triple object. If an identical triple already exists, returns the existing one without duplicating. Subject and predicate must not be NIL. Values can be strings, symbols, keywords, or numbers. Strings are automatically interned for memory efficiency.

```lisp
(let ((g (make-graph)))
  ;; Basic usage
  (add-triple g "alice" "knows" "bob")

  ;; Mixed types
  (add-triple g :alice :age 30)
  (add-triple g "sensor-1" "reading" 3.14)

  ;; Duplicate returns existing triple
  (let ((t1 (add-triple g "a" "b" "c"))
        (t2 (add-triple g "a" "b" "c")))
    (eq t1 t2)))  ; => T
```

---

## remove-triple

```lisp
(remove-triple g subject predicate object) → boolean
```

Remove a specific triple. Returns T if the triple existed and was removed, NIL otherwise.

```lisp
(let ((g (make-graph)))
  (add-triple g "alice" "knows" "bob")
  (remove-triple g "alice" "knows" "bob")   ; => T
  (remove-triple g "alice" "knows" "bob")   ; => NIL (already gone)
  (triple-count g))                          ; => 0
```

---

## remove-triples

```lisp
(remove-triples g &key subject predicate object) → integer
```

Remove all triples matching the given constraints. Returns the number of triples removed.

```lisp
(let ((g (make-graph)))
  (add-triple g "alice" "knows" "bob")
  (add-triple g "alice" "knows" "charlie")
  (add-triple g "alice" "age" 30)

  ;; Remove by subject + predicate
  (remove-triples g :subject "alice" :predicate "knows")
  (triple-count g))  ; => 1 (only "age" remains)
```

---

## get-triples

```lisp
(get-triples g &key subject predicate object) → list-of-triples
```

Query triples using any combination of constraints. Uses the optimal index automatically. With no constraints, returns all triples.

```lisp
(let ((g (make-graph)))
  (add-triple g "alice" "knows" "bob")
  (add-triple g "alice" "knows" "charlie")
  (add-triple g "bob" "knows" "charlie")

  ;; By subject
  (length (get-triples g :subject "alice"))      ; => 2

  ;; By predicate + object
  (length (get-triples g :predicate "knows"
                         :object "charlie"))      ; => 2

  ;; All triples
  (length (get-triples g)))                       ; => 3
```

---

## has-triple-p

```lisp
(has-triple-p g subject predicate object) → boolean
```

Check whether a specific triple exists. O(1) lookup via the SPO index.

```lisp
(let ((g (make-graph)))
  (add-triple g "alice" "knows" "bob")
  (has-triple-p g "alice" "knows" "bob")      ; => T
  (has-triple-p g "alice" "knows" "charlie"))  ; => NIL
```

---

## triplep

```lisp
(triplep x) → boolean
```

Test whether `x` is a triple object.

```lisp
(triplep (add-triple (make-graph) "a" "b" "c"))  ; => T
(triplep '("a" "b" "c"))                          ; => NIL
```

---

## triple-subject

```lisp
(triple-subject triple) → value
```

Return the subject of a triple.

```lisp
(let* ((g (make-graph))
       (tr (add-triple g "alice" "knows" "bob")))
  (triple-subject tr))  ; => "alice"
```

---

## triple-predicate

```lisp
(triple-predicate triple) → value
```

Return the predicate of a triple.

```lisp
(let* ((g (make-graph))
       (tr (add-triple g "alice" "knows" "bob")))
  (triple-predicate tr))  ; => "knows"
```

---

## triple-object

```lisp
(triple-object triple) → value
```

Return the object of a triple.

```lisp
(let* ((g (make-graph))
       (tr (add-triple g "alice" "age" 30)))
  (triple-object tr))  ; => 30
```

---

## all-subjects

```lisp
(all-subjects g) → list
```

Return a list of all unique subjects in the graph.

```lisp
(let ((g (make-graph)))
  (add-triple g "alice" "knows" "bob")
  (add-triple g "alice" "age" 30)
  (add-triple g "bob" "age" 25)
  (all-subjects g))  ; => ("alice" "bob")
```

---

## all-predicates

```lisp
(all-predicates g) → list
```

Return a list of all unique predicates in the graph.

```lisp
(let ((g (make-graph)))
  (add-triple g "alice" "knows" "bob")
  (add-triple g "alice" "age" 30)
  (all-predicates g))  ; => ("knows" "age")
```

---

## all-objects

```lisp
(all-objects g) → list
```

Return a list of all unique objects in the graph.

```lisp
(let ((g (make-graph)))
  (add-triple g "alice" "knows" "bob")
  (add-triple g "alice" "age" 30)
  (all-objects g))  ; => ("bob" 30)
```
