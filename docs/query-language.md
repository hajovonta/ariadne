# Ariadne Query Language Reference

Ariadne provides four complementary query interfaces, each suited to different use cases:

| Interface | Style | Best For |
|-----------|-------|----------|
| [Triple Store Lookups](#1-triple-store--direct-lookups) | Imperative | Simple CRUD, existence checks, enumeration |
| [SPARQL-like Query DSL](#2-sparql-like-query-dsl) | Declarative | Complex joins, filtering, aggregation |
| [Pattern Matching Primitives](#3-pattern-matching-primitives) | Programmatic | Raw binding environments, custom query logic |
| [Gremlin-style Traversal](#4-gremlin-style-traversal) | Navigational | Path finding, graph walking, reachability |

Additional sections cover the [Property Graph Layer](#5-property-graph-layer), [RDF Import/Export](#6-rdf-import--export), [Transactions](#7-transactions), and [Persistence](#8-persistence).

---

## 1. Triple Store — Direct Lookups

The lowest-level query interface. All lookups are index-backed and return lists of triple objects.

### Adding and Removing

```lisp
;; Add a triple (returns the triple object)
(add-triple g "alice" "knows" "bob")

;; Triples can use strings, symbols, numbers, or keywords
(add-triple g :alice :type :person)
(add-triple g "alice" "age" 30)
(add-triple g "alice" "height" 1.65)

;; Duplicates are silently ignored
(add-triple g "alice" "knows" "bob")  ; no-op, returns existing triple

;; Remove a specific triple
(remove-triple g "alice" "knows" "bob")

;; Remove all triples matching a constraint
(remove-triples g :subject "alice")
(remove-triples g :predicate "knows")

;; Clear everything
(clear-graph g)
```

### Querying

```lisp
;; By subject (uses SPO index)
(get-triples g :subject "alice")

;; By predicate (uses POS index)
(get-triples g :predicate "knows")

;; By object (uses OSP index)
(get-triples g :object "bob")

;; Combined constraints
(get-triples g :subject "alice" :predicate "knows")
(get-triples g :predicate "knows" :object "bob")

;; All triples
(get-triples g)

;; Existence check
(has-triple-p g "alice" "knows" "bob")  ; => T or NIL
```

### Triple Accessors

```lisp
(let ((tr (add-triple g "alice" "knows" "bob")))
  (triple-subject tr)    ; => "alice"
  (triple-predicate tr)  ; => "knows"
  (triple-object tr)     ; => "bob"
  (triplep tr))          ; => T
```

### Enumeration

```lisp
(all-subjects g)    ; => ("alice" "bob" ...)
(all-predicates g)  ; => ("knows" "age" ...)
(all-objects g)     ; => ("bob" 30 ...)
(triple-count g)    ; => 42
```

---

## 2. SPARQL-like Query DSL

Declarative queries using pattern matching with logic variables. Variables are symbols starting with `?`.

### Basic SELECT / WHERE

```lisp
;; Single variable
(query g '(select (?who)
           (where ("alice" "knows" ?who))))
;; => (("bob") ("charlie"))

;; Multiple variables
(query g '(select (?person ?age)
           (where (?person "age" ?age))))
;; => (("alice" 30) ("bob" 25) ("charlie" 35))

;; Select all bound variables
(query g '(select *
           (where (?s "knows" ?o))))
```

### Joins

Variables shared across patterns act as join conditions:

```lisp
;; Friend of friend
(query g '(select (?fof)
           (where ("alice" "knows" ?friend)
                  (?friend "knows" ?fof))))
;; => (("charlie"))

;; Three-way join: friend of friend with their age
(query g '(select (?fof ?age)
           (where ("alice" "knows" ?friend)
                  (?friend "knows" ?fof)
                  (?fof "age" ?age))))
;; => (("charlie" 35))

;; Co-workers: people at the same company
(query g '(select (?p1 ?p2)
           (where (?p1 "works-at" ?company)
                  (?p2 "works-at" ?company))))
```

### FILTER

Filter conditions are CL expressions with `?variables` substituted at evaluation time. Any valid CL expression can be used:

```lisp
;; Numeric comparison
(query g '(select (?person)
           (where (?person "age" ?age))
           (filter (> ?age 30))))

;; String matching
(query g '(select (?person)
           (where (?person "name" ?name))
           (filter (search "Smith" ?name))))

;; Equality
(query g '(select (?entity)
           (where (?entity "type" ?type))
           (filter (equal ?type "person"))))

;; Multiple conditions (implicit AND)
(query g '(select (?who)
           (where (?who "age" ?age)
                  (?who "type" ?type))
           (filter (> ?age 28)
                   (equal ?type "person"))))
```

### OPTIONAL

Include results even when the optional pattern doesn't match. Analogous to a SQL LEFT JOIN:

```lisp
(query g '(select (?name ?email)
           (where (?person "name" ?name))
           (optional (?person "email" ?email))))
;; People without email will have NIL for ?email
```

### UNION

Combine results from multiple independent patterns:

```lisp
(query g '(select (?who)
           (union
            (where (?who "knows" "bob"))
            (where (?who "likes" "bob")))))
```

### Aggregation

```lisp
;; Count matching results
(query g '(select ((count ?friend))
           (where ("alice" "knows" ?friend))))
;; => ((3))

;; Distinct values
(query g '(select-distinct (?type)
           (where (?x "type" ?type))))
;; => (("person") ("company"))
```

### ORDER BY / LIMIT / OFFSET

```lisp
;; Sort by age (ascending)
(query g '(select (?person ?age)
           (where (?person "age" ?age))
           (order-by ?age)))

;; Pagination
(query g '(select (?person)
           (where (?person "type" "person"))
           (order-by ?person)
           (limit 10)
           (offset 20)))
```

### Clause Reference

| Clause | Syntax | Description |
|--------|--------|-------------|
| `select` | `(select (?vars...) ...)` | Project specific variables |
| `select *` | `(select * ...)` | Project all bound variables |
| `select-distinct` | `(select-distinct (?vars...) ...)` | Deduplicated results |
| `where` | `(where (s p o) ...)` | Triple patterns to match |
| `filter` | `(filter expr ...)` | Keep results where all exprs are true |
| `optional` | `(optional (s p o) ...)` | Left-join patterns |
| `union` | `(union (where ...) (where ...))` | Combine result sets |
| `order-by` | `(order-by ?var)` | Sort results by variable |
| `limit` | `(limit n)` | Maximum number of results |
| `offset` | `(offset n)` | Skip first n results |
| `count` | `(select ((count ?var)) ...)` | Count matching results |

### Comparison with SPARQL

Ariadne's query DSL shares the same concepts and mental model as W3C SPARQL, expressed as s-expressions rather than a string-based grammar. A SPARQL user will find the mapping intuitive:

```
SPARQL                              Ariadne
─────                               ───────
SELECT ?name ?age                   (select (?name ?age)
WHERE {                              (where (?person "name" ?name)
  ?person foaf:name ?name .                 (?person "age" ?age))
  ?person foaf:age ?age .
  FILTER(?age > 30)                  (filter (> ?age 30)))
}
```

Key differences from SPARQL:
- S-expression syntax instead of string-based grammar — composable, macroexpandable
- Filter expressions are native CL — any Lisp function can be used
- No SPARQL string parser required
- No PREFIX declarations needed (use CL strings or keywords directly)
- `CONSTRUCT`, `DESCRIBE`, `ASK`, property paths, subqueries, and federated queries are not yet implemented

---

## 3. Pattern Matching Primitives

The engine underlying the query DSL. Returns raw binding environments (alists) for programmatic use.

### Variables

```lisp
(variable-p '?x)       ; => T
(variable-p 'alice)    ; => NIL
(variable-p "alice")   ; => NIL
```

### Single Pattern

```lisp
;; Returns a list of binding environments (alists)
(match-pattern g '("alice" "knows" ?x))
;; => (((?X . "bob")) ((?X . "charlie")))

;; All three positions can be variables
(match-pattern g '(?s ?p ?o))
;; => one binding per triple in the graph
```

### Multi-Pattern Joins

```lisp
(match-patterns g '(("alice" "knows" ?friend)
                    (?friend "age" ?age)))
;; => (((?AGE . 25) (?FRIEND . "bob")))
```

### Binding Lookup

```lisp
(let ((env '((?x . "alice") (?y . "bob"))))
  (lookup-binding '?x env)   ; => "alice"
  (lookup-binding '?z env))  ; => NIL
```

### Unification

The core operation that drives pattern matching. Returns two values: the updated environment and a success flag.

```lisp
(unify '?x "alice" nil)              ; => ((?X . "alice")), T
(unify "alice" "alice" nil)          ; => NIL, T   — empty env, success
(unify "alice" "bob" nil)            ; => NIL, NIL — failure
(unify '?x "bob" '((?x . "bob")))   ; => ((?X . "bob")), T — consistent
(unify '?x "bob" '((?x . "alice"))) ; => NIL, NIL — conflict
```

---

## 4. Gremlin-style Traversal

Imperative graph walking with chainable steps. Each step takes the current set of nodes and produces a new set.

### Basic Steps

```lisp
;; Follow outgoing edges
(traverse g "alice" '(out "knows"))
;; => ("bob" "charlie")

;; Follow incoming edges
(traverse g "bob" '(in "knows"))
;; => ("alice")

;; Both directions
(traverse g "alice" '(both "knows"))
;; => ("bob" "charlie" "dave")
```

### Chaining

Steps are applied sequentially — the output of each step becomes the input of the next:

```lisp
;; Two hops: friend of friend
(traverse g "alice" '(out "knows") '(out "knows"))
;; => ("charlie" "dave")

;; Three hops
(traverse g "a" '(out "next") '(out "next") '(out "next"))
;; => ("d")
```

### Filtering

```lisp
;; Filter by comparison
(traverse g "alice"
          '(out "knows")
          '(has "age" (> 30)))
;; => ("charlie")

;; Filter by exact value
(traverse g "alice"
          '(out "knows")
          '(has "type" "person"))
;; => ("bob" "charlie")
```

### Value Extraction

```lisp
;; Get property values from traversal results
(traverse g "alice"
          '(out "knows")
          '(values "age"))
;; => (25 35)
```

### Path Tracking

```lisp
;; Returns full paths instead of just endpoints
(traverse-with-path g "alice" '(out "knows") '(out "knows"))
;; => (("alice" "bob" "charlie"))
```

### Depth-Limited Traversal

```lisp
;; All nodes reachable within N hops (cycle-safe)
(traverse-depth g "alice" "knows" :max-depth 2)
;; => ("bob" "charlie")
```

### Shortest Path

BFS-based shortest path between two nodes:

```lisp
(shortest-path g "alice" "dave" :edge-type "knows")
;; => ("alice" "bob" "dave")

;; Returns NIL when no path exists
(shortest-path g "alice" "isolated" :edge-type "knows")
;; => NIL
```

### Step Reference

| Step | Syntax | Description |
|------|--------|-------------|
| `out` | `(out "predicate")` | Follow outgoing edges |
| `in` | `(in "predicate")` | Follow incoming edges |
| `both` | `(both "predicate")` | Follow edges in both directions |
| `has` | `(has "predicate" value)` | Filter by exact property value |
| `has` | `(has "predicate" (> 30))` | Filter with comparison |
| `values` | `(values "predicate")` | Extract property values |

---

## 5. Property Graph Layer

A higher-level API for working with labeled, typed nodes and edges. Built on top of the triple store using reserved predicates.

### Nodes

```lisp
;; Create with labels and properties
(add-node g "alice" :labels '(:person :employee)
                    :properties '((:name . "Alice") (:age . 30)))

;; Query
(node-property g "alice" :name)      ; => "Alice"
(node-properties g "alice")          ; => ((:NAME . "Alice") (:AGE . 30))
(node-labels g "alice")              ; => (:PERSON :EMPLOYEE)
(get-node g "alice")                 ; => node object or NIL

;; Modify
(set-node-property g "alice" :email "alice@example.com")
(remove-node-property g "alice" :email)

;; Find by label
(find-nodes g :label :person)        ; => list of node objects

;; Remove (also removes all connected edges)
(remove-node g "alice")
```

### Edges

```lisp
;; Create typed edges with properties
(add-edge g "alice" "bob" :knows
          :properties '((:since . 2020) (:weight . 0.9)))

;; Query
(get-edges g :from "alice")                    ; all outgoing
(get-edges g :to "bob")                        ; all incoming
(get-edges g :from "alice" :type :knows)       ; filtered by type

;; Accessors
(let ((e (first (get-edges g :from "alice" :type :knows))))
  (edge-from e)                  ; => "alice"
  (edge-to e)                    ; => "bob"
  (edge-type e)                  ; => :KNOWS
  (edge-property g e :since))    ; => 2020

;; Remove
(remove-edge g "alice" "bob" :knows)
```

### Neighbors

```lisp
(neighbors g "alice" :direction :out)    ; outgoing neighbors
(neighbors g "alice" :direction :in)     ; incoming neighbors
(neighbors g "alice" :direction :both)   ; all neighbors
```

---

## 6. RDF Import / Export

### N-Triples

The simplest RDF serialization — one triple per line.

```lisp
;; Import from string
(import-ntriples g
  "<http://example.org/alice> <http://xmlns.com/foaf/0.1/knows> <http://example.org/bob> .")

;; Import from file
(import-ntriples-file g #p"/path/to/data.nt")

;; Export
(export-ntriples g)
;; => "<http://example.org/alice> <http://xmlns.com/foaf/0.1/knows> <http://example.org/bob> .\n"
```

Supported features:
- URI references (`<http://...>`)
- Typed literals (`"30"^^<xsd:integer>` → parsed as CL integer)
- Language-tagged strings (`"Alice"@en`)
- Blank nodes (`_:b1`)

### Turtle

A more compact RDF syntax with prefix declarations and shorthand.

```lisp
(import-turtle g
  "@prefix foaf: <http://xmlns.com/foaf/0.1/> .
   @prefix ex: <http://example.org/> .

   ex:alice foaf:name \"Alice\" ;
            foaf:age 30 ;
            foaf:knows ex:bob , ex:charlie .")
```

Supported features:
- `@prefix` declarations with automatic expansion
- Semicolon shorthand (same subject, new predicate)
- Comma shorthand (same subject and predicate, new object)
- Quoted strings, numbers, URIs

### N-Quads

N-Triples extended with an optional fourth element (graph name):

```lisp
(import-nquads g
  "<http://example.org/alice> <http://example.org/knows> <http://example.org/bob> <http://example.org/g1> .")
```

### Roundtrip

```lisp
(let ((nt (export-ntriples g1)))
  (import-ntriples g2 nt))
;; g2 now contains the same triples as g1
```

---

## 7. Transactions

### with-transaction (recommended)

Automatically rolls back on error:

```lisp
;; Success — both triples committed
(with-transaction (g)
  (add-triple g "alice" "knows" "bob")
  (add-triple g "bob" "knows" "charlie"))

;; Error — graph unchanged, rolled back to snapshot
(handler-case
    (with-transaction (g)
      (add-triple g "alice" "knows" "dave")
      (error "something went wrong"))
  (error () nil))
```

### Manual Transactions

```lisp
(let ((tx (begin-transaction g)))
  ;; ... make changes ...
  ;; Undo everything:
  (rollback-transaction tx))
```

The snapshot captured at `begin-transaction` can be inspected:

```lisp
(length (transaction-snapshot tx))  ; number of triples at snapshot time
```

---

## 8. Persistence

Save and load graphs to disk. Uses CL's `print`/`read` for full type preservation — strings, numbers, symbols, and keywords all survive the roundtrip.

```lisp
;; Save
(save-graph g #p"/path/to/graph.ariadne")

;; Load
(defparameter *g* (load-graph #p"/path/to/graph.ariadne"))

;; Graph name is preserved
(graph-name *g*)  ; => "social"
```

---

## Complete API Reference

### Graph

| Function | Signature | Description |
|----------|-----------|-------------|
| `make-graph` | `(&key name)` | Create a new empty graph |
| `graphp` | `(x)` | Test if x is a graph |
| `graph-name` | `(graph)` | Get graph name |
| `triple-count` | `(graph)` | Number of triples |
| `clear-graph` | `(graph)` | Remove all triples |

### Triples

| Function | Signature | Description |
|----------|-----------|-------------|
| `add-triple` | `(g s p o)` | Add a triple (deduplicates) |
| `remove-triple` | `(g s p o)` | Remove a specific triple |
| `remove-triples` | `(g &key subject predicate object)` | Remove matching triples |
| `get-triples` | `(g &key subject predicate object)` | Query triples |
| `has-triple-p` | `(g s p o)` | Check existence |
| `triplep` | `(x)` | Test if x is a triple |
| `triple-subject` | `(triple)` | Get subject |
| `triple-predicate` | `(triple)` | Get predicate |
| `triple-object` | `(triple)` | Get object |
| `all-subjects` | `(g)` | All unique subjects |
| `all-predicates` | `(g)` | All unique predicates |
| `all-objects` | `(g)` | All unique objects |

### Query DSL

| Function | Signature | Description |
|----------|-----------|-------------|
| `query` | `(g expr)` | Execute a query expression |

### Pattern Matching

| Function | Signature | Description |
|----------|-----------|-------------|
| `variable-p` | `(x)` | Test if x is a `?variable` |
| `lookup-binding` | `(var env)` | Look up variable in bindings |
| `unify` | `(pattern value env)` | Unify pattern with value |
| `match-pattern` | `(g pattern)` | Match single triple pattern |
| `match-patterns` | `(g patterns)` | Match and join multiple patterns |

### Property Graph

| Function | Signature | Description |
|----------|-----------|-------------|
| `add-node` | `(g id &key properties labels)` | Create a node |
| `get-node` | `(g id)` | Get node or NIL |
| `remove-node` | `(g id)` | Remove node and its edges |
| `node-id` | `(node)` | Get node ID |
| `node-property` | `(g id prop)` | Get a node property |
| `set-node-property` | `(g id prop value)` | Set a node property |
| `remove-node-property` | `(g id prop)` | Remove a node property |
| `node-properties` | `(g id)` | All properties as alist |
| `node-labels` | `(g id)` | All labels |
| `find-nodes` | `(g &key label)` | Find nodes by label |
| `add-edge` | `(g from to type &key properties)` | Create a typed edge |
| `remove-edge` | `(g from to type)` | Remove an edge |
| `get-edges` | `(g &key from to type)` | Query edges |
| `edge-from` | `(edge)` | Edge source |
| `edge-to` | `(edge)` | Edge target |
| `edge-type` | `(edge)` | Edge type |
| `edge-property` | `(g edge prop)` | Get an edge property |
| `neighbors` | `(g id &key direction type)` | Get neighbor nodes |

### Traversal

| Function | Signature | Description |
|----------|-----------|-------------|
| `traverse` | `(g start &rest steps)` | Chainable graph traversal |
| `traverse-with-path` | `(g start &rest steps)` | Traversal returning full paths |
| `traverse-depth` | `(g start pred &key max-depth)` | Depth-limited traversal |
| `shortest-path` | `(g from to &key edge-type)` | BFS shortest path |

### Transactions

| Function | Signature | Description |
|----------|-----------|-------------|
| `with-transaction` | `((graph) &body body)` | Macro: auto-rollback on error |
| `begin-transaction` | `(g)` | Start a transaction |
| `rollback-transaction` | `(tx)` | Rollback to snapshot |

### Import / Export

| Function | Signature | Description |
|----------|-----------|-------------|
| `import-ntriples` | `(g string)` | Import N-Triples from string |
| `import-ntriples-file` | `(g path)` | Import N-Triples from file |
| `export-ntriples` | `(g)` | Export as N-Triples string |
| `import-turtle` | `(g string)` | Import Turtle from string |
| `export-turtle` | `(g)` | Export as Turtle string |
| `import-nquads` | `(g string)` | Import N-Quads from string |

### Persistence

| Function | Signature | Description |
|----------|-----------|-------------|
| `save-graph` | `(g path)` | Save graph to file |
| `load-graph` | `(path)` | Load graph from file |
