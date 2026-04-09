# Inference

Forward-chaining rule engine that materializes new triples by pattern matching.

[← Back to API Reference](../api-reference.md)

---

## defrule

```lisp
(defrule g name &key when then) → name
```

Define an inference rule. `:when` is a list of triple patterns (with `?variables`) that must all match. `:then` is a list of triple templates that will be instantiated with the matched bindings and added to the graph.

```lisp
;; Simple: parent implies ancestor
(defrule g :ancestor
  :when '((?a "parent" ?b))
  :then '((?a "ancestor" ?b)))

;; Transitive: ancestor chains
(defrule g :ancestor-chain
  :when '((?a "ancestor" ?b) (?b "ancestor" ?c))
  :then '((?a "ancestor" ?c)))

;; RDFS-style subclass inference
(defrule g :subclass
  :when '((?x "type" ?class) (?class "subClassOf" ?super))
  :then '((?x "type" ?super)))
```

---

## remove-rule

```lisp
(remove-rule g name) → boolean
```

Remove a previously defined rule by name.

```lisp
(defrule g :symmetric
  :when '((?a "friendOf" ?b))
  :then '((?b "friendOf" ?a)))
(remove-rule g :symmetric)
(graph-rules g)  ; => NIL (if no other rules)
```

---

## apply-rules

```lisp
(apply-rules g) → integer
```

Apply all defined rules repeatedly until no new triples are generated (fixed-point evaluation). Returns the number of new triples materialized.

```lisp
(add-triple g "alice" "parent" "bob")
(add-triple g "bob" "parent" "charlie")
(defrule g :ancestor
  :when '((?a "parent" ?b))
  :then '((?a "ancestor" ?b)))
(defrule g :ancestor-chain
  :when '((?a "ancestor" ?b) (?b "ancestor" ?c))
  :then '((?a "ancestor" ?c)))
(apply-rules g)
(has-triple-p g "alice" "ancestor" "charlie")  ; => T
```

---

## graph-rules

```lisp
(graph-rules g) → list
```

Return a list of all rules defined on the graph.

```lisp
(defrule g :rule-1 :when '((?a "p" ?b)) :then '((?b "q" ?a)))
(defrule g :rule-2 :when '((?a "q" ?b)) :then '((?a "r" ?b)))
(length (graph-rules g))  ; => 2
```
