# Query DSL

The declarative SPARQL-like query interface.

[← Back to API Reference](../api-reference.md)

---

## query

```lisp
(query g expr) → results
```

Execute a query expression against graph `g`. The expression type determines the return value:

- `select` / `select-distinct` → list of result rows (lists of values)
- `ask` → T or NIL
- `construct` → list of (subject predicate object) lists
- `describe` → list of triples

The query DSL supports WHERE, FILTER, OPTIONAL, UNION, NOT EXISTS, MINUS, BIND, VALUES, GROUP BY, HAVING, ORDER BY, LIMIT, OFFSET, subqueries, property paths, and aggregation functions (count, sum, avg, min, max, group_concat, sample).

```lisp
;; SELECT: find friends and their ages
(let ((g (make-graph)))
  (add-triple g "alice" "knows" "bob")
  (add-triple g "bob" "age" 25)
  (query g '(select (?friend ?age)
             (where ("alice" "knows" ?friend)
                    (?friend "age" ?age)))))
;; => (("bob" 25))

;; ASK: boolean existence check
(query g '(ask (where ("alice" "knows" "bob"))))
;; => T

;; CONSTRUCT: generate new triples from patterns
(query g '(construct (?a "friend-of-friend" ?c)
           (where (?a "knows" ?b)
                  (?b "knows" ?c))))
;; => (("alice" "friend-of-friend" "charlie"))
```

See [Query Language Reference](../query-language.md) for the full syntax guide with all clauses, property paths, and aggregation.
