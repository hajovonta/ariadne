# Ariadne

A **graph database** in Common Lisp with a SPARQL-like query DSL, Gremlin-style traversal, RDF import/export, property graph support, inference rules, and Graphviz export. Built with TDD — 345/345 tests passing.

## Key Features

- **Triple Store** — Core storage model based on subject-predicate-object triples with three concurrent indexes (SPO, POS, OSP) for O(1) lookups on any combination
- **SPARQL-like Query DSL** — Declarative pattern matching with logic variables: `SELECT`, `ASK`, `CONSTRUCT`, `DESCRIBE`, `WHERE`, `FILTER`, `OPTIONAL`, `UNION`, `NOT EXISTS`, `MINUS`, `BIND`, `VALUES`, subqueries
- **Aggregation** — `GROUP BY` with `HAVING`, `COUNT`, `SUM`, `AVG`, `MIN`, `MAX`
- **Property Paths** — Transitive closure (`+`), Kleene star (`*`), zero-or-one (`?`), inverse (`inv`, `inv+`), alternative (`alt`), bounded (`range`)
- **Gremlin-style Traversal** — Imperative graph walking with chainable steps: `out`, `in`, `both`, `has`, `values`
- **Property Graph Layer** — Nodes with labels and properties, typed edges with properties, neighbor queries
- **Inference Rules** — Forward-chaining rule engine with fixed-point evaluation: RDFS-style subclass reasoning, symmetric properties, transitive closure
- **RDF Import/Export** — N-Triples parser/serializer, Turtle parser (token-based), N-Quads import
- **Graphviz Export** — DOT format output with predicate filtering, subgraph extraction around a node, file export
- **Transactions** — Snapshot-based rollback with `with-transaction` macro
- **Persistence** — Save/load graphs to disk with full type preservation
- **Graph Algorithms** — BFS shortest path, depth-limited traversal, cycle-safe walking, path tracking
- **Safe Filters** — Whitelisted filter evaluation with regex support (cl-ppcre)

## Quick Start

```lisp
(ql:quickload :ariadne)
(use-package :ariadne)

;; Create a graph and add some data
(defparameter *g* (make-graph :name "social"))

(add-triple *g* "alice" "knows" "bob")
(add-triple *g* "bob" "knows" "charlie")
(add-triple *g* "alice" "age" 30)
(add-triple *g* "bob" "age" 25)
(add-triple *g* "charlie" "age" 35)

;; Query: who does alice know?
(query *g* '(select (?who)
             (where ("alice" "knows" ?who))))
;; => (("bob") ("charlie"))

;; Query: friends of friends with age > 30
(query *g* '(select (?fof ?age)
             (where ("alice" "knows" ?friend)
                    (?friend "knows" ?fof)
                    (?fof "age" ?age))
             (filter (> ?age 30))))
;; => (("charlie" 35))

;; Transitive closure: all reachable people
(query *g* '(select (?person)
             (where ("alice" (+ "knows") ?person))))
;; => (("bob") ("charlie"))

;; Inference: derive new knowledge
(defrule *g* :symmetric-knows
  :when '((?a "knows" ?b))
  :then '((?b "knows" ?a)))
(apply-rules *g*)
;; Now: (has-triple-p *g* "bob" "knows" "alice") => T

;; Graphviz export
(export-dot *g* :file #p"social.dot")
```

## Installation

```bash
cd ~/quicklisp/local-projects/
git clone <repository-url> ariadne
```

```lisp
(ql:quickload :ariadne)
```

## Dependencies

- **cl-ppcre** — Regular expressions for REGEX filter support

## Running Tests

```lisp
(ql:quickload :ariadne-tests)
(ariadne/tests:run-all-tests)

;; Run a specific suite
(ariadne/tests:run-suite :triple-store)
(ariadne/tests:run-suite :query-dsl)
(ariadne/tests:run-suite :traversal)
(ariadne/tests:run-suite :inference)
```

### Test Suites

| Suite | Description |
|-------|-------------|
| `:triple-store` | Core add/remove/query operations, deduplication, enumeration |
| `:indexing` | SPO/POS/OSP index consistency and lookups |
| `:property-graph` | Nodes, edges, labels, properties, neighbors |
| `:query-dsl` | SELECT/WHERE/FILTER/OPTIONAL/UNION/ORDER BY/LIMIT |
| `:query-advanced` | GROUP BY, HAVING, ASK, CONSTRUCT, BIND, NOT EXISTS, MINUS, property paths |
| `:query-extended` | Inverse paths, Kleene star, DESCRIBE, REGEX |
| `:subqueries` | VALUES inline data, subqueries in WHERE and FILTER |
| `:sparql-patterns` | Pattern matching, multi-pattern joins, unification |
| `:traversal` | Graph walking, path tracking, shortest path, cycle detection |
| `:transactions` | Commit, rollback, snapshot isolation |
| `:inference` | Forward-chaining rules, RDFS subclass, symmetric, transitive, fixed-point |
| `:graph-export` | DOT/Graphviz export, predicate filter, subgraph, file output |
| `:import-export` | N-Triples, Turtle, N-Quads parsing and serialization |
| `:persistence` | Save/load with type preservation |
| `:edge-cases` | Unicode, emoji, empty graphs, stress tests, mixed types |

## Architecture

### Storage Model

Everything is stored as triples. The property graph layer uses reserved predicates (`:ariadne/type`, `:ariadne/label`, `:ariadne/prop/*`, etc.) to represent nodes, edges, and properties within the same triple store.

### Indexing

Three hash-table indexes provide O(1) lookups:

```
SPO: subject → predicate → object → triple    (find by subject)
POS: predicate → object → subject → triple     (find by predicate)
OSP: object → subject → predicate → triple     (find by object)
```

Any combination of bound/unbound positions is efficiently served by choosing the appropriate index.

### Query Execution

```
Query DSL expression
  → Parse clauses (where, filter, optional, union, bind, values, ...)
  → Expand property paths
  → Pattern matching with index-backed triple lookup
  → Logic variable unification and join
  → Subquery evaluation
  → NOT EXISTS / MINUS exclusion
  → BIND computed variables
  → Filter evaluation (safe, whitelisted)
  → GROUP BY + aggregation + HAVING
  → Projection, ordering, pagination
  → Results
```

### Module Structure

| File | Description |
|------|-------------|
| `ariadne.lisp` | Core triple store, indexes, graph operations |
| `pattern.lisp` | Logic variables, unification, pattern matching |
| `query.lisp` | SPARQL-like query DSL, property paths, subqueries, aggregation |
| `property-graph.lisp` | Node/edge/property layer |
| `traversal.lisp` | Gremlin-style traversal, shortest path |
| `transactions.lisp` | Snapshot-based transactions |
| `inference.lisp` | Forward-chaining rule engine |
| `graph-export.lisp` | DOT/Graphviz export |
| `import-export.lisp` | N-Triples, Turtle, N-Quads |
| `persistence.lisp` | Save/load to disk |

## Documentation

- **[Ariadne Query Language Reference](docs/query-language.md)** — Comprehensive guide to all query interfaces, with examples and complete API reference

## License

MIT
