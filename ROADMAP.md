# Ariadne Roadmap

## Current Status

- **472 tests passing** across 29 test suites
- ~95% SPARQL 1.1 feature coverage
- W3C Turtle positive conformance: 213/213 (100%)
- W3C Turtle negative conformance: 4/78 (5%) — parser is too lenient with invalid input
- Successfully imports real-world Turtle files (perihelion-kg.ttl — 604 lines, 1551 triples)
- Import throughput: ~500K triples/sec (Turtle), ~80-360K triples/sec (N-Triples/N-Quads)
- Published at [git.sr.ht/~hajovonta/ariadne](https://git.sr.ht/~hajovonta/ariadne)

## Phase 1 — Polish for Release

- [x] REPL result formatting — pretty-print query results as aligned tables
- [x] Example datasets — family tree with example queries in `examples/`
- [x] Proper Turtle export — prefix grouping, semicolons, commas, 'a' shorthand
- [x] Error handling — graceful errors on malformed input, bad queries, unclosed strings
- [x] LICENSE file
- [x] Remote repository (git.sr.ht/~hajovonta/ariadne)

## Phase 2 — W3C Turtle Strict Conformance

Target: 78/78 negative tests correctly rejected.

- [ ] URI validation — reject spaces, invalid characters in `<...>`
- [ ] Structure validation — require subject, predicate, object; reject unterminated statements
- [ ] String validation — reject unterminated quotes, bad escape sequences
- [ ] Prefix validation — reject undefined prefixes, malformed prefix declarations
- [ ] Number validation — reject malformed numeric literals
- [ ] Keyword validation — reject invalid keywords (e.g. `@PREFIX` instead of `@prefix`)
- [ ] N3 extras rejection — reject N3 syntax that isn't valid Turtle (e.g. `=`, `=>`, `{...}`)

## Phase 3 — Performance

- [x] Streaming import — line-by-line `stream-import-nquads` / `stream-import-ntriples` for large files
- [x] String interning — deduplicate repeated URIs to reduce memory
- [ ] Compact index — replace nested hash tables with flat structures for lower memory overhead
- [ ] Benchmarks at scale — test at 1M+ triples with `--dynamic-space-size 16384`
- [x] Query planner — reorder WHERE patterns by selectivity for optimal execution
- [ ] Thread safety — read-write locks for concurrent access (bordeaux-threads)

### Current Scale Limits

- ~500K triples: comfortable on default SBCL heap (8GB)
- ~1-2M triples: possible with `--dynamic-space-size 16384`
- 4M+ triples: needs compact index or disk-backed storage

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
- [ ] REPL integration — table formatting, graph visualization

## Phase 6 — Distribution

- [ ] Quicklisp submission
- [ ] Documentation site with tutorials
- [ ] CI / automated test runs

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
