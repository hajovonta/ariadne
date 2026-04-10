# Ariadne Roadmap

## Current Status

- **647 tests passing** across 46 test suites
- ~95% SPARQL 1.1 feature coverage
- W3C Turtle positive conformance: 209/209 (100%)
- W3C Turtle negative conformance: 82/82 (100%)
- Successfully imports real-world Turtle files (perihelion-kg.ttl — 604 lines, 1551 triples)
- Import throughput: ~500K triples/sec (Turtle), ~100K triples/sec (N-Quads streaming)
- Scale: 3.6M triples in 34 seconds (drugbank full)
- Published at [git.sr.ht/~hajovonta/ariadne](https://git.sr.ht/~hajovonta/ariadne)

## Phase 1 — Polish for Release

- [x] REPL result formatting — pretty-print query results as aligned tables
- [x] Example datasets — family tree with example queries in `examples/`
- [x] Proper Turtle export — prefix grouping, semicolons, commas, 'a' shorthand
- [x] Error handling — graceful errors on malformed input, bad queries, unclosed strings
- [x] LICENSE file
- [x] Remote repository (git.sr.ht/~hajovonta/ariadne)

## Phase 2 — W3C Turtle Strict Conformance ✓

Target: 82/82 negative tests correctly rejected. Achieved.

- [x] URI validation — reject spaces, invalid characters, bad escapes in `<...>`
- [x] Structure validation — require subject, predicate, object; reject unterminated statements
- [x] String validation — reject unterminated quotes, bad escape sequences
- [x] Prefix validation — reject undefined prefixes, malformed prefix declarations
- [x] Number validation — reject malformed numeric literals
- [x] Keyword validation — reject invalid keywords (e.g. `@PREFIX` instead of `@prefix`)
- [x] N3 extras rejection — reject N3 syntax that isn't valid Turtle (e.g. `=`, `=>`, `{...}`)
- [x] Blank node validation — reject labels ending with dot, blank nodes as predicates
- [x] NQuads-in-Turtle detection — reject 4th element after triple

## Phase 3 — Performance

- [x] Streaming import — line-by-line `stream-import-nquads` / `stream-import-ntriples` for large files
- [x] String interning — deduplicate repeated URIs to reduce memory
- [x] Compact index — flat composite-key hash tables (41-63% less GC)
- [x] Benchmarks at scale — tested at 3.6M triples (drugbank full)
- [x] Query planner — reorder WHERE patterns by selectivity for optimal execution
- [x] Thread safety — lock-based concurrent access (bordeaux-threads)

### Current Scale Limits

- ~500K triples: comfortable on default SBCL heap
- ~1.7M triples: 18 seconds (clinicaltrials 2M lines)
- ~3.6M triples: 34 seconds (drugbank full 4.2M lines)
- Throughput stable at ~100K triples/sec across all scales

## Phase 4 — Remaining SPARQL Features

- [x] Subqueries — queries nested inside WHERE clauses (partially done, needs hardening)
- [x] Sequence paths `/` — `(seq "knows" "name")`
- [x] GROUP_CONCAT aggregation
- [x] SAMPLE aggregation
- [x] Named graphs — GRAPH clause, add-quad, get-quads, named-graphs, N-Quads with graph names

## Phase 5 — Differentiation

- [x] Reactive queries / triggers — `(on-match g :name :pattern '(...) :callback fn)`
- [x] Graph analytics — PageRank, connected components, degree centrality, clustering coefficient
- [x] SPARQL string parser — accept standard SPARQL query strings, translate to DSL
- [x] REPL integration — graph visualization via Graphviz, describe-graph summary

## Phase 6 — Distribution

- [ ] Quicklisp submission
- [ ] Documentation site with tutorials
- [x] CI / automated test runs — builds.sr.ht with .build.yml

## Phase 7 — Interactive Web Visualization ✓

- [x] Hunchentoot web server serving graph explorer
- [x] Cytoscape.js frontend with interactive zoom, pan, drag
- [x] JSON endpoint exporting graph/subgraph as Cytoscape elements
- [x] Layout options: force-directed, hierarchical, circular
- [x] Predicate filtering — show/hide edge types
- [x] Node hover: show properties and connections
- [x] Subgraph extraction: explore neighborhood around a clicked node
- [x] Search: find nodes by label or URI

## Phase 8 — Graph Operations & Export Formats ✓

- [x] merge-graphs — merge two graphs, deduplicating triples
- [x] diff-graphs — find triples in one graph but not the other
- [x] copy-graph — deep copy a graph
- [x] export-nquads — N-Quads export with graph names
- [x] export-json-ld — JSON-LD export
- [x] export-cytoscape-json — Cytoscape.js compatible JSON

## Phase 9 — Data Quality & Validation ✓

- [x] Schema validation — define expected predicates/types, validate triples on insert
- [x] Constraint checking — cardinality constraints (e.g. "a person must have exactly one name")
- [x] Duplicate detection — find near-duplicate entities via fuzzy string matching on labels

## Phase 10 — Interoperability ✓

- [x] SPARQL endpoint — HTTP server accepting standard SPARQL protocol queries
- [x] OWL reasoning — RDFS/OWL entailment beyond simple forward-chaining rules

## Phase 11 — Scale & Operations

- [x] Disk-backed persistence — memory-mapped storage for graphs larger than RAM
- [x] Incremental persistence — append-only transaction log with crash recovery
- [ ] Graph partitioning — split large graphs across multiple in-memory stores
- [x] Profiling — query timing, index stats, bottleneck identification
- [x] Backup/restore with versioning — timestamped snapshots
- [x] Query result pagination — cursor-based iteration over large result sets

## Phase 12 — Query Completeness ✓

- [x] SPARQL UPDATE — INSERT DATA, DELETE DATA
- [x] SPARQL UPDATE protocol — HTTP endpoint at /update
- [x] SPARQL parser: CONSTRUCT support
- [x] SPARQL parser: GROUP BY, HAVING, aggregation functions
- [x] SPARQL parser: OPTIONAL, UNION, BIND
- [x] SPARQL parser: property paths (+, *, ^)

## Phase 13 — Import Formats ✓

- [x] RDF/XML import
- [x] JSON-LD import

## Phase 14 — CI & Distribution

- [x] CI pipeline — builds.sr.ht, runs on every push
- [ ] Quicklisp submission
- [ ] Documentation site with tutorials

## Phase 15 — Usability & Query Completeness

- [x] Prefix registry — `register-prefix` so queries don't need full URIs
- [x] DELETE WHERE — bulk delete by pattern matching
- [x] SPARQL parser: DESCRIBE support
- [x] Blank node skolemization — stable IDs for blank nodes
- [x] Memory usage reporting — RAM per graph

## Phase 16 — Advanced Features

- [x] Full-text search — index literals, search by keyword
- [x] Graph versioning / temporal queries — query graph at point in time
- [x] SPARQL SERVICE — federated queries to remote endpoints
- [ ] SHACL validation — W3C standard shape constraints
- [x] Graph events / webhooks — notify external systems on changes

## Completed

- [x] Core triple store with SPO/POS/OSP indexes
- [x] SPARQL-like query DSL (SELECT, ASK, CONSTRUCT, DESCRIBE)
- [x] Pattern matching with logic variable unification
- [x] WHERE, FILTER, OPTIONAL, UNION, NOT EXISTS, MINUS, BIND, VALUES
- [x] Subqueries in WHERE and FILTER
- [x] GROUP BY, HAVING, COUNT, SUM, AVG, MIN, MAX
- [x] ORDER BY, LIMIT, OFFSET, DISTINCT
- [x] Property paths: transitive (+), Kleene star (*), zero-or-one (?), inverse (inv, inv+), alternative (alt), bounded (range)
- [x] Safe filter evaluation with whitelisted operations
- [x] REGEX filter (cl-ppcre)
- [x] Gremlin-style traversal (out/in/both/has/values)
- [x] Path tracking, depth-limited traversal, BFS shortest path
- [x] Property graph layer (nodes, edges, labels, properties)
- [x] Forward-chaining inference rules with fixed-point evaluation
- [x] N-Triples import/export
- [x] Turtle import (W3C conformant — 213/213 positive tests)
- [x] N-Quads import
- [x] DOT/Graphviz export with predicate filtering and subgraph extraction
- [x] Snapshot-based transactions with rollback
- [x] Persistence via CL print/read
- [x] 'a' shorthand, boolean literals, comments in Turtle
- [x] Real-world Turtle file import (perihelion-kg.ttl)
- [x] MIT LICENSE
- [x] Remote repository (git.sr.ht/~hajovonta/ariadne)
- [x] Comprehensive documentation (README.md, docs/query-language.md)
- [x] REPL result formatting (format-results with aligned tables)
- [x] Proper Turtle export with prefix detection, shorthand, roundtrip
- [x] Streaming import for large N-Triples/N-Quads files
- [x] Error handling for malformed input, bad queries, unclosed strings
- [x] Benchmarked: 500K triples/sec Turtle, 86-112K triples/sec N-Quads
- [x] Graph analytics: PageRank, connected components, degree centrality, clustering coefficient
- [x] Example dataset: family tree with queries and inference
- [x] Sequence paths (seq "p1" "p2" ...)
- [x] GROUP_CONCAT and SAMPLE aggregations
- [x] Reactive triggers: on-match, remove-trigger, graph-triggers
- [x] Named graphs: add-quad, get-quads, GRAPH clause, N-Quads import with graph names
- [x] SPARQL string parser: SELECT, ASK, PREFIX, FILTER, DISTINCT, LIMIT, joins
