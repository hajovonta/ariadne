# Pattern Matching

Low-level pattern matching primitives underlying the query DSL.

[← Back to API Reference](../api-reference.md)

---

## variable-p

```lisp
(variable-p x) → boolean
```

Test whether `x` is a logic variable — a symbol whose name starts with `?`.

```lisp
(variable-p '?x)       ; => T
(variable-p '?person)  ; => T
(variable-p 'alice)    ; => NIL
(variable-p "?x")      ; => NIL (strings are not variables)
```

---

## lookup-binding

```lisp
(lookup-binding var env) → value-or-nil
```

Look up a variable's value in a binding environment (alist). Returns NIL if unbound.

```lisp
(let ((env '((?x . "alice") (?y . "bob"))))
  (lookup-binding '?x env)   ; => "alice"
  (lookup-binding '?y env)   ; => "bob"
  (lookup-binding '?z env))  ; => NIL
```

---

## unify

```lisp
(unify pattern value env) → (values new-env success-p)
```

Attempt to unify `pattern` with `value` under the given binding environment. Returns two values: the updated environment and a success flag. Important: NIL env with T success means empty bindings (match with no variables), not failure.

```lisp
;; Bind a variable
(unify '?x "alice" nil)
;; => ((?X . "alice")), T

;; Constants match
(unify "alice" "alice" nil)
;; => NIL, T  (success, no new bindings)

;; Constants conflict
(unify "alice" "bob" nil)
;; => NIL, NIL  (failure)

;; Consistent rebinding
(unify '?x "alice" '((?x . "alice")))
;; => ((?X . "alice")), T

;; Conflicting binding
(unify '?x "bob" '((?x . "alice")))
;; => NIL, NIL  (failure)
```

---

## match-pattern

```lisp
(match-pattern g pattern) → list-of-environments
```

Match a single triple pattern against the graph. Returns a list of binding environments (alists), one per matching triple.

```lisp
(let ((g (make-graph)))
  (add-triple g "alice" "knows" "bob")
  (add-triple g "alice" "knows" "charlie")

  ;; One variable
  (match-pattern g '("alice" "knows" ?who))
  ;; => (((?WHO . "bob")) ((?WHO . "charlie")))

  ;; Multiple variables
  (match-pattern g '(?s "knows" ?o))
  ;; => (((?O . "bob") (?S . "alice")) ((?O . "charlie") (?S . "alice")))

  ;; No match
  (match-pattern g '("dave" "knows" ?x)))
  ;; => NIL
```

---

## match-patterns

```lisp
(match-patterns g patterns) → list-of-environments
```

Match multiple triple patterns with join semantics. Shared variables across patterns act as join conditions. Returns binding environments satisfying all patterns.

```lisp
(let ((g (make-graph)))
  (add-triple g "alice" "knows" "bob")
  (add-triple g "bob" "age" 25)
  (add-triple g "alice" "knows" "charlie")
  (add-triple g "charlie" "age" 35)

  ;; Join: friends with their ages
  (match-patterns g '(("alice" "knows" ?friend)
                      (?friend "age" ?age)))
  ;; => (((?AGE . 25) (?FRIEND . "bob"))
  ;;     ((?AGE . 35) (?FRIEND . "charlie")))

  ;; Three-way join
  (match-patterns g '(("alice" "knows" ?f)
                      (?f "age" ?age)
                      (?f "knows" ?fof))))
```
