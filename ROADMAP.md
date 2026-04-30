# Ariadne Roadmap

## Current Status (v0.1.0)

- **798 unit tests passing** across 46 test suites
- **W3C SPARQL 1.1**: 328/328 (100%)
- **W3C SHACL Core**: 98/98 (100%)
- **W3C SHACL-SPARQL**: 23/23 (100%)
- **W3C Turtle**: 209/209 positive, 82/82 negative (100%)
- Import throughput: ~500K triples/sec (Turtle), ~100K triples/sec (N-Quads streaming)
- Scale: 3.6M triples in 34 seconds (drugbank full)
- Published at [git.sr.ht/~hajovonta/ariadne](https://git.sr.ht/~hajovonta/ariadne)
- SHACL EARL report submitted to W3C: [data-shapes#854](https://github.com/w3c/data-shapes/pull/854)

## Distribution

- [x] CI pipeline — builds.sr.ht, runs on every push
- [x] SHACL EARL report submitted to W3C
- [ ] Quicklisp submission — [requested](https://github.com/quicklisp/quicklisp-projects/issues/2572), waiting
- [ ] Documentation site with tutorials

## Future Directions

### Performance & Scale
- [ ] Lazy/streaming query evaluation — avoid materializing full result sets
- [ ] Index compression — reduce memory footprint for large graphs
- [ ] Parallel query execution — leverage multiple cores for independent BGPs
- [ ] Disk-backed B-tree indexes — handle graphs larger than RAM without full materialization

### Standards & Interoperability
- [ ] SPARQL 1.1 Update: INSERT/DELETE with WHERE patterns (currently only INSERT DATA/DELETE DATA/DELETE WHERE)
- [ ] SPARQL 1.1 Graph Store HTTP Protocol
- [ ] RDF-star / SPARQL-star — quoted triples for statement-level metadata
- [ ] OWL 2 RL profile — extended reasoning beyond current RDFS/OWL subset
- [ ] TriG import/export

### Usability
- [ ] SPARQL query explain/plan — show execution strategy
- [ ] Interactive REPL with tab completion for predicates/subjects
- [ ] Graph diff visualization
- [ ] Import progress reporting for large files

### Ecosystem
- [ ] Portability testing — verify on CCL, ABCL (SBCL and ECL confirmed working)
- [ ] Benchmark suite — reproducible comparisons with other triplestores
- [ ] Example applications — knowledge graph construction, data validation pipelines

## Completed Phases

### Phase 1 — Core & Polish ✓
Triple store, query DSL, pattern matching, Gremlin traversal, property graph layer, inference rules, Turtle/N-Triples/N-Quads import/export, DOT export, transactions, persistence, REPL formatting, error handling.

### Phase 2 — W3C Turtle Conformance ✓
209/209 positive, 82/82 negative tests. Full URI/string/number/blank node/keyword validation.

### Phase 3 — Performance ✓
Streaming import, string interning, compact indexes, query planner, thread safety. 3.6M triples tested.

### Phase 4 — SPARQL Features ✓
Subqueries, sequence paths, GROUP_CONCAT, SAMPLE, named graphs, GRAPH clause.

### Phase 5 — Differentiation ✓
Reactive triggers, graph analytics (PageRank, connected components, degree centrality, clustering), SPARQL string parser, Graphviz visualization.

### Phase 6 — Web & Visualization ✓
Hunchentoot server, Cytoscape.js frontend, JSON endpoints, layout options, predicate filtering, search.

### Phase 7 — Export & Operations ✓
merge/diff/copy graphs, N-Quads/JSON-LD/Cytoscape JSON export.

### Phase 8 — Validation ✓
Schema validation, cardinality constraints, duplicate detection.

### Phase 9 — Interoperability ✓
SPARQL HTTP endpoint, OWL/RDFS reasoning, RDF/XML import, JSON-LD import.

### Phase 10 — Scale & Operations ✓
Disk-backed persistence, transaction log, graph partitioning, profiling, backup/restore, pagination.

### Phase 11 — Query Completeness ✓
SPARQL UPDATE, CONSTRUCT, GROUP BY/HAVING, OPTIONAL/UNION/BIND, property paths, DELETE WHERE, DESCRIBE, SERVICE federation.

### Phase 12 — SHACL ✓
Full W3C SHACL Core (98/98) and SHACL-SPARQL (23/23). Custom constraint components, pre-binding per B.3.4.2, ASK/SELECT validators.

### Phase 13 — Full SPARQL 1.1 Conformance ✓
328/328 W3C tests. Algebra-based evaluator, FROM clause, CONSTRUCT collections, GROUP BY expression scope, RDF collections in WHERE, all 14 categories at 100%.
