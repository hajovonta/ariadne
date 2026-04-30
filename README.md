# Ariadne

A **graph database** in Common Lisp with full W3C SPARQL 1.1 and SHACL conformance, Gremlin-style traversal, RDF import/export, property graph support, inference rules, graph analytics, and Graphviz visualization. Built with TDD — 798/798 tests passing.

## Key Features

### Storage & Indexing
- **Triple Store** — Compact flat indexes (7 hash tables) for O(1) lookups. Tested to 3.6M triples.
- **Thread Safety** — Lock-based concurrent read/write access (bordeaux-threads)
- **Disk-Backed Persistence** — Append-only txlog backing store with auto-persist on every mutation
- **Graph Partitioning** — Hash-based subject partitioning across N stores with fan-out queries
- **Named Graphs** — Quad store with GRAPH clause for multi-graph queries

### Query & Pattern Matching
- **W3C SPARQL 1.1** — Full conformance: 328/328 tests (100%). SELECT, ASK, CONSTRUCT, DESCRIBE, WHERE, FILTER, OPTIONAL, UNION, NOT EXISTS, MINUS, BIND, VALUES, GRAPH, subqueries, FROM
- **SPARQL String Parser** — Execute standard SPARQL query strings directly, including `DESCRIBE`
- **SPARQL UPDATE** — `INSERT DATA`, `DELETE DATA`, `DELETE WHERE` via string parser and HTTP endpoint
- **SPARQL SERVICE** — Federated queries to remote SPARQL endpoints via Drakma
- **Aggregation** — `GROUP BY` with `HAVING`, `COUNT`, `SUM`, `AVG`, `MIN`, `MAX`, `GROUP_CONCAT`, `SAMPLE`
- **Property Paths** — Transitive (`+`), Kleene star (`*`), zero-or-one (`?`), inverse, alternative, bounded, sequence
- **Full-Text Search** — Inverted index over literals with case-insensitive multi-word AND matching

### Graph Models
- **Property Graph Layer** — Nodes with labels and properties, typed edges, neighbor queries
- **Gremlin-style Traversal** — Chainable steps: `out`, `in`, `both`, `has`, `values`, path tracking, shortest path

### Reasoning & Validation
- **Inference Rules** — Forward-chaining rule engine with fixed-point evaluation
- **OWL/RDFS Reasoning** — subClassOf, subPropertyOf, domain/range, inverseOf, transitiveProperty, symmetricProperty, sameAs
- **Schema Validation** — Class/property type and cardinality constraints
- **SHACL Validation** — W3C Shapes Constraint Language: 98/98 core + 23/23 SPARQL tests (100% conformance)
- **Duplicate Detection** — Jaccard bigram similarity for near-duplicate entities

### Import & Export
- **RDF Import** — N-Triples, Turtle (W3C conformant: 209/209 positive, 82/82 negative), N-Quads, RDF/XML, JSON-LD
- **RDF Export** — N-Triples, Turtle, N-Quads, JSON-LD, Cytoscape JSON, DOT/Graphviz
- **Streaming Import** — Line-by-line import for large files (~100K triples/sec)
- **Blank Node Skolemization** — Replace blank nodes with stable `/.well-known/genid/` URIs

### Visualization & Web
- **Graphviz Rendering** — dot/neato/fdp/circo/twopi/sfdp engines with predicate filtering
- **Web Explorer** — Interactive Cytoscape.js graph viewer with type-colored nodes, predicate filtering, node details panel, right-click context menu (expand/collapse/hide/pin), search, multiple layouts
- **SPARQL Endpoint** — HTTP server at `/sparql` and `/update` (Hunchentoot)

### Operations & Durability
- **Transactions** — Snapshot-based rollback with `with-transaction` macro
- **Persistence** — Save/load graphs with full type preservation
- **Transaction Log** — Append-only log for incremental persistence and crash recovery
- **Backup/Restore** — Timestamped versioned snapshots
- **Graph Versioning** — Named checkpoints with temporal queries via `query-at-version`
- **Graph Events/Webhooks** — Callbacks and HTTP POST notifications on graph mutations (Drakma)
- **Query Pagination** — Cursor-based iteration over large result sets
- **Profiling** — Query timing, graph statistics, memory usage reporting
- **Graph Analytics** — PageRank, connected components, degree centrality, clustering coefficient
- **Prefix Registry** — Register short prefixes for use across all operations

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

Tested on SBCL and ECL.

## Dependencies

- **cl-ppcre** — Regular expressions (SPARQL REGEX, Turtle parsing, SHACL pattern matching)
- **bordeaux-threads** — Thread-safe concurrent access
- **hunchentoot** — SPARQL HTTP endpoint
- **drakma** — SPARQL SERVICE federation, graph event webhooks
- **com.inuoe.jzon** — JSON-LD import/export, SPARQL JSON results
- **local-time** — xsd:dateTime handling in SHACL and typed literals
- **ironclad** — Cryptographic hash functions (MD5, SHA1, SHA256, SHA384, SHA512)
- **babel** — Portable string-to-octets encoding for hash digest inputs
- **cxml-stp** — RDF/XML parsing

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
| `:w3c-turtle` | W3C Turtle conformance (209/209 positive, 82/82 negative) |
| `:streaming-import` | Line-by-line N-Triples/N-Quads import for large files |
| `:repl-formatting` | Pretty-print query results as aligned tables |
| `:error-handling` | Graceful errors for malformed input, bad queries, persistence |
| `:examples` | Example datasets and queries |
| `:graph-analytics` | PageRank, connected components, degree centrality, clustering |
| `:remaining-sparql` | Sequence paths, GROUP_CONCAT, SAMPLE |
| `:reactive` | Reactive triggers on pattern match |
| `:persistence` | Save/load with type preservation |
| `:edge-cases` | Unicode, emoji, empty graphs, stress tests, mixed types |
| `:named-graphs` | Named graphs: quads, GRAPH clause |
| `:sparql-parser` | SPARQL string parser |
| `:sparql-parser-extended` | CONSTRUCT, OPTIONAL, UNION, GROUP BY, HAVING, OFFSET |
| `:sparql-parser-paths` | Property paths and BIND in SPARQL parser |
| `:sparql-filter-extended` | FILTER: boolean operators, NOT, BOUND, isLiteral, true/false |
| `:sparql-update` | SPARQL UPDATE: INSERT DATA, DELETE DATA |
| `:sparql-endpoint` | SPARQL HTTP endpoint |
| `:compact-index` | Compact index correctness |
| `:thread-safety` | Concurrent reads and writes |
| `:visualization` | Graph visualization via Graphviz |
| `:graph-operations` | Merge, diff, copy, export formats |
| `:web-server` | Web visualization server |
| `:owl-reasoning` | OWL/RDFS entailment rules |
| `:schema-validation` | Schema validation and constraints |
| `:export-formats` | JSON-LD and Cytoscape JSON export |
| `:rdf-xml` | RDF/XML import |
| `:json-ld-import` | JSON-LD import |
| `:profiling` | Query profiling and graph statistics |
| `:incremental-persistence` | Append-only transaction log |
| `:pagination` | Query result pagination |
| `:backup` | Backup/restore with versioning |
| `:prefix-registry` | Prefix registry for short URIs |
| `:memory-usage` | Memory usage reporting |
| `:skolemization` | Blank node skolemization |
| `:delete-where` | DELETE WHERE and SPARQL DESCRIBE |
| `:full-text-search` | Full-text search over literals |
| `:graph-versioning` | Graph versioning and temporal queries |

### W3C Conformance

| Standard | Tests | Result |
|----------|-------|--------|
| SPARQL 1.1 | 328/328 | 100% |
| Turtle (syntax) | 209/209 positive, 82/82 negative | 100% |
| SHACL Core | 98/98 | 100% |
| SHACL-SPARQL | 23/23 | 100% |

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
