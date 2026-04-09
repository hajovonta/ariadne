# Reactive Triggers

Register callbacks that fire when triples matching a pattern are added to the graph.

[← Back to API Reference](../api-reference.md)

---

## on-match

```lisp
(on-match g name &key pattern callback) → name
```

Register a trigger. When a triple is added that matches `pattern`, `callback` is called with the triple and the binding environment. The pattern uses `?variables` like query patterns.

```lisp
;; Alert on new people
(on-match g :new-person
  :pattern '(?person "type" "person")
  :callback (lambda (triple env)
              (format t "New person: ~A~%"
                      (cdr (assoc '?person env)))))
(add-triple g "alice" "type" "person")
;; prints: New person: alice

;; Log all new triples with a specific predicate
(on-match g :audit-knows
  :pattern '(?a "knows" ?b)
  :callback (lambda (triple env)
              (format t "~A now knows ~A~%"
                      (cdr (assoc '?a env))
                      (cdr (assoc '?b env)))))

;; Cascading: trigger adds more triples (which may fire other triggers)
(on-match g :auto-symmetric
  :pattern '(?a "friendOf" ?b)
  :callback (lambda (triple env)
              (add-triple g
                          (cdr (assoc '?b env))
                          "friendOf"
                          (cdr (assoc '?a env)))))
```

---

## remove-trigger

```lisp
(remove-trigger g name) → boolean
```

Remove a trigger by name.

```lisp
(on-match g :my-trigger
  :pattern '(?s ?p ?o)
  :callback (lambda (tr env) (declare (ignore tr env))))
(remove-trigger g :my-trigger)
```

---

## graph-triggers

```lisp
(graph-triggers g) → list
```

Return a list of all triggers defined on the graph.

```lisp
(on-match g :t1 :pattern '(?s "p" ?o) :callback #'identity)
(on-match g :t2 :pattern '(?s "q" ?o) :callback #'identity)
(length (graph-triggers g))  ; => 2
```
