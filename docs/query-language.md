# Ariadne Query Language Reference

Ariadne provides four complementary query interfaces, each suited to different use cases:

| Interface | Style | Best For |
|-----------|-------|----------|
| [Triple Store Lookups](#1-triple-store--direct-lookups) | Imperative | Simple CRUD, existence checks, enumeration |
| [SPARQL-like Query DSL](#2-sparql-like-query-dsl) | Declarative | Complex joins, filtering, aggregation |
| [Pattern Matching Primitives](#3-pattern-matching-primitives) | Programmatic | Raw binding environments, custom query logic |
| [Gremlin-style Traversal](#4-gremlin-style-traversal) | Navigational | Path finding, graph walking, reachability |

Additional sections cover the [Property Graph Layer](#5-property-graph-layer), [RDF Import/Export](#6-rdf-import--export), [Transactions](#7-transactions), [Persistence](#8-persistence), and [SHACL Validation](#13-shacl-validation).

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

Filter conditions use a safe, whitelisted subset of CL operations with `?variables` substituted at evaluation time:

```lisp
;; Numeric comparison
(query g '(select (?person)
           (where (?person "age" ?age))
           (filter (> ?age 30))))

;; String matching
(query g '(select (?person)
           (where (?person "name" ?name))
           (filter (search "Smith" ?name))))

;; Regex (requires cl-ppcre)
(query g '(select (?person)
           (where (?person "email" ?email))
           (filter (regex ?email "example\\.com$"))))

;; Case-insensitive regex
(query g '(select (?person)
           (where (?person "name" ?name))
           (filter (regex ?name "smith" :case-insensitive-mode))))

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

Allowed filter operations: `< > <= >= = /= + - * /`, `equal equalp eql`, `string= string-equal search string< string>`, `numberp stringp symbolp integerp floatp`, `concatenate`, `not`, `regex`.

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

### NOT EXISTS

Exclude results where a pattern matches:

```lisp
;; People without an email address
(query g '(select (?person)
           (where (?person "type" "person"))
           (not-exists (?person "email" ?any))))
```

### MINUS

Remove results where shared variables match:

```lisp
;; People alice knows but doesn't dislike
(query g '(select (?who)
           (where ("alice" "knows" ?who))
           (minus ("alice" "dislikes" ?who))))
```

### BIND

Assign computed values to new variables:

```lisp
(query g '(select (?person ?age)
           (where (?person "birth-year" ?year))
           (bind ?age (- 2026 ?year))))

;; String concatenation
(query g '(select (?person ?full)
           (where (?person "first-name" ?first)
                  (?person "last-name" ?last))
           (bind ?full (concatenate 'string ?first " " ?last))))
```

### VALUES

Restrict query to specific variable bindings (inline data):

```lisp
;; Single variable
(query g '(select (?person ?age)
           (where (?person "age" ?age))
           (values ?person ("alice" "charlie"))))

;; Multiple variables
(query g '(select (?a ?b)
           (where (?a "knows" ?b))
           (values (?a ?b) (("alice" "bob") ("bob" "dave")))))
```

### Subqueries

Subqueries can appear in WHERE patterns or FILTER expressions:

```lisp
;; In WHERE: find the person with the maximum age
(query g '(select (?person ?age)
           (where (?person "age" ?age)
                  (subquery (select ((max ?a))
                             (where (?anyone "age" ?a)))
                            ?age))))

;; In FILTER: people with above-average salary
(query g '(select (?person ?sal)
           (where (?person "salary" ?sal))
           (filter (> ?sal
                     (subquery (select ((avg ?s))
                                (where (?x "salary" ?s))))))))
```

### ASK

Boolean existence check — returns T or NIL:

```lisp
(query g '(ask (where ("alice" "knows" "bob"))))   ; => T
(query g '(ask (where ("alice" "knows" "nobody")))) ; => NIL
```

### CONSTRUCT

Generate new triples from query results:

```lisp
;; Returns list of (s p o) lists
(query g '(construct (?a "friend-of-friend" ?c)
           (where (?a "knows" ?b)
                  (?b "knows" ?c))))

;; Insert directly into a target graph
(query g `(construct (?a "friend-of-friend" ?c)
           (where (?a "knows" ?b)
                  (?b "knows" ?c))
           (into ,target-graph)))
```

### DESCRIBE

Return all triples about a resource:

```lisp
(query g '(describe "alice"))            ; triples where alice is subject OR object
(query g '(describe "alice" :subject))   ; only where alice is subject
```

### Aggregation

```lisp
;; Count (without GROUP BY)
(query g '(select ((count ?friend))
           (where ("alice" "knows" ?friend))))
;; => ((3))

;; Any aggregation works without GROUP BY
(query g '(select ((max ?age))
           (where (?person "age" ?age))))
;; => ((35))

;; Distinct values
(query g '(select-distinct (?type)
           (where (?x "type" ?type))))
;; => (("person") ("company"))
```

### GROUP BY / HAVING

```lisp
;; Group by with count
(query g '(select (?company (count ?person))
           (where (?person "works-at" ?company))
           (group-by ?company)))
;; => (("acme" 3) ("globex" 1))

;; Multiple aggregations
(query g '(select (?team (min ?score) (max ?score))
           (where (?person "team" ?team)
                  (?person "score" ?score))
           (group-by ?team)))

;; HAVING filters on aggregated values
(query g '(select (?company (count ?person))
           (where (?person "works-at" ?company))
           (group-by ?company)
           (having (> (count ?person) 1))))
```

Supported aggregation functions: `count`, `sum`, `avg`, `min`, `max`, `group_concat`, `sample`.

### GROUP_CONCAT / SAMPLE

```lisp
;; Concatenate all names in a group
(query g '(select (?team (group_concat ?name))
           (where (?person "team" ?team)
                  (?person "name" ?name))
           (group-by ?team)))
;; => (("red" "Alice,Bob") ("blue" "Charlie"))

;; Pick an arbitrary value from a group
(query g '(select (?team (sample ?name))
           (where (?person "team" ?team)
                  (?person "name" ?name))
           (group-by ?team)))
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
| `ask` | `(ask (where ...))` | Boolean existence check |
| `construct` | `(construct (s p o) (where ...))` | Generate triples |
| `describe` | `(describe resource)` | All triples about a resource |
| `where` | `(where (s p o) ...)` | Triple patterns to match |
| `filter` | `(filter expr ...)` | Keep results where all exprs are true |
| `regex` | `(regex ?var "pattern")` | Regex filter (in filter clause) |
| `optional` | `(optional (s p o) ...)` | Left-join patterns |
| `union` | `(union (where ...) (where ...))` | Combine result sets |
| `not-exists` | `(not-exists (s p o) ...)` | Exclude matching patterns |
| `minus` | `(minus (s p o) ...)` | Remove matching bindings |
| `bind` | `(bind ?var expr)` | Computed variable assignment |
| `values` | `(values ?var (v1 v2 ...))` | Inline data / restrict bindings |
| `subquery` | `(subquery (select ...) ?var)` | Nested query in WHERE or FILTER |
| `group-by` | `(group-by ?var)` | Group results for aggregation |
| `having` | `(having expr)` | Filter on aggregated values |
| `order-by` | `(order-by ?var)` | Sort results by variable |
| `limit` | `(limit n)` | Maximum number of results |
| `offset` | `(offset n)` | Skip first n results |

### Property Path Syntax

| Path | Syntax | Description |
|------|--------|-------------|
| Transitive | `(+ "pred")` | One or more hops |
| Kleene star | `(* "pred")` | Zero or more hops |
| Zero-or-one | `(? "pred")` | Zero or one hop |
| Inverse | `(inv "pred")` | Follow edges backwards |
| Inverse transitive | `(inv+ "pred")` | Backwards, one or more hops |
| Alternative | `(alt "p1" "p2")` | Match any of the predicates |
| Bounded | `(range "pred" min max)` | Between min and max hops |
| Sequence | `(seq "p1" "p2")` | Follow p1 then p2 in order |

```lisp
;; Transitive: all reachable nodes
(query g '(select (?person) (where ("alice" (+ "knows") ?person))))

;; Kleene star: include start node
(query g '(select (?node) (where ("alice" (* "knows") ?node))))

;; Inverse: who knows bob?
(query g '(select (?who) (where ("bob" (inv "knows") ?who))))

;; All ancestors (inverse transitive)
(query g '(select (?ancestor) (where ("dave" (inv+ "parent") ?ancestor))))

;; Alternative predicates
(query g '(select (?person) (where ("alice" (alt "knows" "likes") ?person))))

;; Bounded: 1 to 2 hops
(query g '(select (?node) (where ("a" (range "knows" 1 2) ?node))))

;; Sequence: follow "knows" then "name"
(query g '(select (?name) (where ("alice" (seq "knows" "name") ?name))))
```

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
- Filter expressions use a safe, whitelisted evaluator — no arbitrary code execution
- SPARQL string parser also available via `(sparql g "SELECT ...")` — full W3C conformance (328/328 tests)
- No PREFIX declarations needed in DSL (use CL strings or keywords directly)
- Named graphs supported via GRAPH clause and add-quad/get-quads

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

---

## 9. Inference Rules

Forward-chaining rule engine that materializes new triples by pattern matching. Rules are applied repeatedly until no new triples are generated (fixed point).

### Defining Rules

```lisp
;; Simple rule: parent implies ancestor
(defrule g :ancestor
  :when '((?a "parent" ?b))
  :then '((?a "ancestor" ?b)))

;; Transitive rule: ancestor chains
(defrule g :ancestor-transitive
  :when '((?a "ancestor" ?b) (?b "ancestor" ?c))
  :then '((?a "ancestor" ?c)))

;; RDFS-style subclass inference
(defrule g :subclass-type
  :when '((?x "type" ?class) (?class "subClassOf" ?super))
  :then '((?x "type" ?super)))

;; Symmetric property
(defrule g :symmetric-friend
  :when '((?a "friendOf" ?b))
  :then '((?b "friendOf" ?a)))
```

### Applying Rules

```lisp
;; Apply all rules until fixed point
(apply-rules g)
```

### Managing Rules

```lisp
;; List all rules
(graph-rules g)

;; Remove a rule
(remove-rule g :ancestor)
```

---

## 10. Graphviz Export

Export graphs to DOT format for visualization with Graphviz.

### Basic Export

```lisp
;; Get DOT string
(export-dot g)
;; => "digraph ariadne { ... }"

;; Named graph
(export-dot (make-graph :name "social"))
;; => "digraph social { ... }"

;; Write to file
(export-dot g :file #p"graph.dot")
```

### Filtering

```lisp
;; Only include specific predicates
(export-dot g :predicates '("knows" "likes"))
```

### Subgraph Extraction

```lisp
;; Export neighborhood around a node (1 hop)
(export-dot g :center "bob" :depth 1)
```

### Rendering

```bash
# Generate PNG from DOT file
dot -Tpng graph.dot -o graph.png

# Generate SVG
dot -Tsvg graph.dot -o graph.svg
```

---

## 11. Named Graphs

Named graphs allow partitioning triples into separate contexts, useful for provenance tracking and dataset management.

### Adding Quads

```lisp
;; Add a triple with a graph name (quad)
(add-quad g "alice" "knows" "bob" "http://example.org/social")
(add-quad g "alice" "age" 30 "http://example.org/personal")
```

### Querying

```lisp
;; Get all triples in a named graph
(get-quads g :graph "http://example.org/social")

;; List all named graphs
(named-graphs g)
;; => ("http://example.org/social" "http://example.org/personal")

;; GRAPH clause in queries
(query g '(select (?who)
           (where (graph "http://example.org/social"
                         (?who "knows" "bob")))))
```

---

## 12. SPARQL String Parser

Execute standard SPARQL 1.1 query strings directly. Full W3C conformance: 328/328 tests (100%).

```lisp
;; SELECT query
(sparql g "SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }")

;; With PREFIX
(sparql g "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
           SELECT ?name WHERE { ?person foaf:name ?name }")

;; ASK query
(sparql g "ASK { <http://example.org/alice> <http://example.org/knows> <http://example.org/bob> }")

;; FILTER, DISTINCT, LIMIT, ORDER BY
(sparql g "SELECT DISTINCT ?name WHERE {
             ?person foaf:name ?name .
             ?person foaf:age ?age .
             FILTER(?age > 30)
           } ORDER BY ?name LIMIT 10")
```

---

## 13. SHACL Validation

W3C Shapes Constraint Language for validating RDF graphs against shape definitions. Full conformance: 98/98 core tests, 23/23 SPARQL tests.

```lisp
;; Validate a graph containing both data and shapes
(shacl-validate g)
;; => (:CONFORMS T :RESULTS NIL)

;; Non-conforming graph returns violation details
(shacl-validate g)
;; => (:CONFORMS NIL :RESULTS ((:TYPE "http://www.w3.org/ns/shacl#Violation"
;;                               :FOCUS-NODE "http://example.org/alice"
;;                               :PATH "http://example.org/age"
;;                               :SOURCE-SHAPE "http://example.org/PersonShape"
;;                               :MESSAGE "...")))
```

Supported constraint types:
- **Value type**: `sh:class`, `sh:datatype`, `sh:nodeKind`
- **Cardinality**: `sh:minCount`, `sh:maxCount`
- **Value range**: `sh:minExclusive`, `sh:maxExclusive`, `sh:minInclusive`, `sh:maxInclusive`
- **String**: `sh:minLength`, `sh:maxLength`, `sh:pattern`, `sh:flags`
- **Property pair**: `sh:equals`, `sh:disjoint`, `sh:lessThan`, `sh:lessThanOrEquals`
- **Logical**: `sh:not`, `sh:and`, `sh:or`, `sh:xone`
- **Shape-based**: `sh:node`, `sh:property`, `sh:qualifiedValueShape`
- **Other**: `sh:in`, `sh:hasValue`, `sh:closed`, `sh:ignoredProperties`, `sh:languageIn`, `sh:uniqueLang`
- **SPARQL-based**: `sh:sparql` with SELECT and ASK validators, custom `sh:ConstraintComponent` with parameter binding per spec B.3.4.2

---

## 14. Reactive Triggers

Register callbacks that fire when triples matching a pattern are added.

```lisp
;; Register a trigger
(on-match g :alert-new-person
  :pattern '(?person "type" "person")
  :callback (lambda (triple env)
              (format t "New person: ~A~%" (cdr (assoc '?person env)))))

;; Now adding a matching triple fires the callback
(add-triple g "alice" "type" "person")
;; prints: New person: alice

;; Remove a trigger
(remove-trigger g :alert-new-person)
```

---

## 15. Graph Analytics

Built-in graph algorithms operating on the triple store.

### PageRank

```lisp
(let ((g (make-graph)))
  (add-triple g "a" "links" "b")
  (add-triple g "b" "links" "c")
  (add-triple g "c" "links" "a")
  (pagerank g :predicate "links" :iterations 20 :damping 0.85))
;; => (("a" . 0.33) ("b" . 0.33) ("c" . 0.33))
```

### Connected Components

```lisp
(connected-components g :predicate "knows")
;; => (("alice" "bob" "charlie") ("dave" "eve"))
```

### Degree Centrality

```lisp
(degree-centrality g :predicate "knows")
;; => (("bob" . 4) ("alice" . 3) ("charlie" . 2))
```

### Clustering Coefficient

```lisp
(clustering-coefficient g :predicate "knows")
;; => (("alice" . 0.67) ("bob" . 0.33) ...)
```

---

## 16. Streaming Import

Line-by-line import for large files that don't fit in memory as strings.

```lisp
;; Stream N-Triples from file
(stream-import-ntriples g #p"/path/to/large-file.nt")

;; Stream N-Quads from file
(stream-import-nquads g #p"/path/to/large-file.nq")
```

Tested at 3.6M triples (drugbank, 34 seconds) and 1.7M triples (clinical trials, 18 seconds).

---

## 17. Visualization

### Graphviz Rendering

Render graphs directly to PNG/SVG/PDF using Graphviz layout engines.

```lisp
;; Basic rendering
(visualize-graph g :file #p"graph.png")

;; Choose layout engine
(visualize-graph g :file #p"graph.png" :engine :neato)    ; force-directed
(visualize-graph g :file #p"graph.svg" :engine :twopi)    ; radial
(visualize-graph g :file #p"graph.pdf" :engine :circo)    ; circular

;; With filtering
(visualize-graph g :file #p"orbits.png"
                    :predicates '("orbits" "hasMoon")
                    :center "Sol" :depth 2
                    :engine :twopi)

;; Open in viewer
(visualize-graph g :file #p"graph.png" :open t)
```

Available engines: `:dot` (hierarchical), `:neato` (force-directed), `:fdp` (spring), `:circo` (circular), `:twopi` (radial), `:sfdp` (scalable).

### Graph Summary

```lisp
(format t "~A" (describe-graph g))
;; Perihelion Knowledge Graph
;; 1551 triples, 245 subjects, 28 predicates, 892 unique objects
;; Top predicates:
;;   rdf:type (332)
;;   rdfs:label (280)
;;   p:definedIn (120)
```

### REPL Table Formatting

```lisp
(format-results
  '(("alice" 30) ("bob" 25) ("charlie" 35))
  '("name" "age"))
;; +---------+-----+
;; | name    | age |
;; +---------+-----+
;; | alice   |  30 |
;; | bob     |  25 |
;; | charlie |  35 |
;; +---------+-----+
```

---

## 18. Graph Operations

### Merge

```lisp
;; Create a new merged graph
(let ((merged (merge-graphs g1 g2)))
  (triple-count merged))

;; Merge into an existing graph
(merge-graphs-into target source)
```

### Diff

```lisp
;; Triples in g1 but not g2
(diff-graphs g1 g2)
;; => list of triple objects
```

### Copy

```lisp
;; Independent deep copy
(let ((g2 (copy-graph g1)))
  (add-triple g1 "new" "triple" "here")
  (triple-count g2))  ; unchanged
```

### N-Quads Export

```lisp
;; Export with graph names preserved
(export-nquads g)
```

---

## 19. Thread Safety

All graph mutations (`add-triple`, `remove-triple`) are protected by a lock. Multiple threads can safely read and write to the same graph concurrently.

```lisp
;; Safe concurrent writes from multiple threads
(let ((threads (loop for i below 4
                     collect (bt:make-thread
                              (lambda ()
                                (dotimes (j 1000)
                                  (add-triple g (format nil "node-~A" j) "type" "node")))))))
  (mapc #'bt:join-thread threads))
```

---

For the complete function reference, see [API Reference](api-reference.md).
