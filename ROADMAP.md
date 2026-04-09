# Ariadne Roadmap

## Current Status

- **376 tests passing** across 18 test suites
- ~90% SPARQL 1.1 feature coverage
- W3C Turtle positive conformance: 213/213 (100%)
- W3C Turtle negative conformance: 4/78 (5%) — parser is too lenient with invalid input
- Successfully imports real-world Turtle files (perihelion-kg.ttl — 604 lines, 1551 triples)
- Import throughput: ~500K triples/sec (Turtle), ~80-360K triples/sec (N-Triples/N-Quads)
- Published at [git.sr.ht/~hajovonta/ariadne](https://git.sr.ht/~hajovonta/ariadne)

## Phase 1 — Polish for Release

- [ ] REPL result formatting — pretty-print query results as aligned tables
- [ ] Example datasets — ship a small knowledge graph with example queries in `examples/`
- [ ] Proper Turtle export — prefix grouping and shorthand (current export falls back to N-Triples)
- [ ] Error handling — graceful errors on malformed input, missing nodes, bad query syntax
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

- [ ] Streaming import — line-by-line `import-nquads-file` / `import-ntriples-file` for multi-GB files
- [ ] String interning — deduplicate repeated URIs to reduce memory
- [ ] Benchmarks at scale — test at 500K and 1M+ triples
- [ ] Query planner — reorder WHERE patterns to minimize intermediate results
- [ ] Thread safety — read-write locks for concurrent access (bordeaux-threads)

## Phase 4 — Remaining SPARQL Features

- [ ] Subqueries — queries nested inside WHERE clauses (partially done, needs hardening)
- [ ] Sequence paths `/` — `foaf:knows/foaf:name` (workaround: multi-pattern joins)
- [ ] GROUP_CONCAT aggregation
- [ ] SAMPLE aggregation
- [ ] Named graphs — GRAPH clause, FROM / FROM NAMED

## Phase 5 — Differentiation

- [ ] Reactive queries / triggers — `(on-match g pattern callback)` fires when a pattern appears
- [ ] Graph analytics — PageRank, connected components, betweenness centrality
- [ ] SPARQL string parser — accept standard SPARQL query strings, translate to DSL
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
