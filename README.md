# Ariadne

A **graph database** in Common Lisp with a SPARQL-like query DSL, Gremlin-style traversal, RDF import/export, property graph support, inference rules, graph analytics, and Graphviz visualization. Built with TDD — 654/654 tests passing.

## Key Features

- **Triple Store** — Core storage with compact flat indexes for O(1) lookups. Scales to 3.6M triples.
- **SPARQL-like Query DSL** — Declarative pattern matching with logic variables: `SELECT`, `ASK`, `CONSTRUCT`, `DESCRIBE`, `WHERE`, `FILTER`, `OPTIONAL`, `UNION`, `NOT EXISTS`, `MINUS`, `BIND`, `VALUES`, subqueries
- **Aggregation** — `GROUP BY` with `HAVING`, `COUNT`, `SUM`, `AVG`, `MIN`, `MAX`, `GROUP_CONCAT`, `SAMPLE`
- **Property Paths** — Transitive closure (`+`), Kleene star (`*`), zero-or-one (`?`), inverse (`inv`, `inv+`), alternative (`alt`), bounded (`range`), sequence (`seq`)
- **Gremlin-style Traversal** — Imperative graph walking with chainable steps: `out`, `in`, `both`, `has`, `values`
- **Property Graph Layer** — Nodes with labels and properties, typed edges with properties, neighbor queries
- **Inference Rules** — Forward-chaining rule engine with fixed-point evaluation
- **RDF Import/Export** — N-Triples, Turtle (W3C conformant: 209/209 positive, 82/82 negative), N-Quads
- **Visualization** — Graphviz rendering (dot/neato/fdp/circo/twopi/sfdp), describe-graph summary
- **Graph Operations** — Merge, diff, copy graphs. N-Quads export.
- **Transactions** — Snapshot-based rollback with `with-transaction` macro
- **Persistence** — Save/load graphs to disk with full type preservation
- **Graph Analytics** — PageRank, connected components, degree centrality, clustering coefficient
- **Reactive Queries** — Triggers that fire callbacks when matching triples are added
- **Named Graphs** — Quad store with GRAPH clause for multi-graph queries
- **SPARQL String Parser** — Execute standard SPARQL query strings directly
- **Thread Safety** — Lock-based concurrent read/write access (bordeaux-threads)
- **Streaming Import** — Line-by-line import for large files (~100K triples/sec)
- **OWL/RDFS Reasoning** — Automatic entailment: subClassOf, subPropertyOf, domain/range, inverseOf, transitiveProperty, symmetricProperty, sameAs
- **Schema Validation** — Define expected classes/properties, validate types and cardinality constraints
- **Duplicate Detection** — Find near-duplicate entities via fuzzy string matching
- **Web Visualization** — Interactive Cytoscape.js graph explorer with predicate filtering, search, layout switching
- **SPARQL Endpoint** — HTTP server accepting SPARQL queries at `/sparql?query=...`
- **Export Formats** — N-Triples, Turtle, N-Quads, JSON-LD, Cytoscape JSON, DOT/Graphviz
- **Import Formats** — N-Triples, Turtle, N-Quads, RDF/XML, JSON-LD
- **SPARQL UPDATE** — INSERT DATA, DELETE DATA via string parser and HTTP endpoint
- **Transaction Log** — Append-only log for incremental persistence and crash recovery
- **Backup/Restore** — Timestamped versioned snapshots with restore-latest
- **Query Pagination** — Cursor-based iteration over large result sets
- **Profiling** — Query timing and graph statistics

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
| `:turtle-real-world` | Real-world Turtle: 'a' shorthand, booleans, long literals, comments |
| `:turtle-export` | Proper Turtle export with prefixes, semicolons, commas |
| `:w3c-turtle` | W3C Turtle conformance (213/213 positive tests) |
| `:streaming-import` | Line-by-line N-Triples/N-Quads import for large files |
| `:repl-formatting` | Pretty-print query results as aligned tables |
| `:error-handling` | Graceful errors for malformed input, bad queries, persistence |
| `:examples` | Example datasets and queries |
| `:graph-analytics` | PageRank, connected components, degree centrality, clustering |
| `:remaining-sparql` | Sequence paths, GROUP_CONCAT, SAMPLE |
| `:reactive` | Reactive triggers on pattern match |
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
| `analytics.lisp` | PageRank, connected components, degree centrality, clustering |
| `reactive.lisp` | Reactive triggers on pattern match |
| `import-export.lisp` | N-Triples, Turtle import/export, N-Quads |
| `streaming.lisp` | Line-by-line streaming import for large files |
| `repl.lisp` | REPL result formatting |
| `persistence.lisp` | Save/load to disk |

## Documentation

- **[Ariadne Query Language Reference](docs/query-language.md)** — Comprehensive guide to all query interfaces, with examples and complete API reference

## License

MIT
