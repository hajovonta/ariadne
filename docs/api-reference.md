# Ariadne API Reference

Complete function reference for the Ariadne graph database. Click section headers for detailed documentation with examples.

---

## [Graph](api/graph.md)

| Function | Signature | Description |
|----------|-----------|-------------|
| [`make-graph`](api/graph.md#make-graph) | `(&key name)` | Create a new empty graph |
| [`graphp`](api/graph.md#graphp) | `(x)` | Test if x is a graph |
| [`graph-name`](api/graph.md#graph-name) | `(graph)` | Get graph name |
| [`triple-count`](api/graph.md#triple-count) | `(graph)` | Number of triples |
| [`clear-graph`](api/graph.md#clear-graph) | `(graph)` | Remove all triples |

## [Triples](api/triples.md)

| Function | Signature | Description |
|----------|-----------|-------------|
| [`add-triple`](api/triples.md#add-triple) | `(g s p o)` | Add a triple (deduplicates) |
| [`remove-triple`](api/triples.md#remove-triple) | `(g s p o)` | Remove a specific triple |
| [`remove-triples`](api/triples.md#remove-triples) | `(g &key subject predicate object)` | Remove matching triples |
| [`get-triples`](api/triples.md#get-triples) | `(g &key subject predicate object)` | Query triples |
| [`has-triple-p`](api/triples.md#has-triple-p) | `(g s p o)` | Check existence |
| [`triplep`](api/triples.md#triplep) | `(x)` | Test if x is a triple |
| [`triple-subject`](api/triples.md#triple-subject) | `(triple)` | Get subject |
| [`triple-predicate`](api/triples.md#triple-predicate) | `(triple)` | Get predicate |
| [`triple-object`](api/triples.md#triple-object) | `(triple)` | Get object |
| [`all-subjects`](api/triples.md#all-subjects) | `(g)` | All unique subjects |
| [`all-predicates`](api/triples.md#all-predicates) | `(g)` | All unique predicates |
| [`all-objects`](api/triples.md#all-objects) | `(g)` | All unique objects |

## [Query DSL](api/query-dsl.md)

| Function | Signature | Description |
|----------|-----------|-------------|
| [`query`](api/query-dsl.md#query) | `(g expr)` | Execute a query expression |

## [Pattern Matching](api/pattern-matching.md)

| Function | Signature | Description |
|----------|-----------|-------------|
| [`variable-p`](api/pattern-matching.md#variable-p) | `(x)` | Test if x is a `?variable` |
| [`lookup-binding`](api/pattern-matching.md#lookup-binding) | `(var env)` | Look up variable in bindings |
| [`unify`](api/pattern-matching.md#unify) | `(pattern value env)` | Unify pattern with value |
| [`match-pattern`](api/pattern-matching.md#match-pattern) | `(g pattern)` | Match single triple pattern |
| [`match-patterns`](api/pattern-matching.md#match-patterns) | `(g patterns)` | Match and join multiple patterns |

## [Property Graph](api/property-graph.md)

| Function | Signature | Description |
|----------|-----------|-------------|
| [`add-node`](api/property-graph.md#add-node) | `(g id &key properties labels)` | Create a node |
| [`get-node`](api/property-graph.md#get-node) | `(g id)` | Get node or NIL |
| [`remove-node`](api/property-graph.md#remove-node) | `(g id)` | Remove node and its edges |
| [`node-id`](api/property-graph.md#node-id) | `(node)` | Get node ID |
| [`node-property`](api/property-graph.md#node-property) | `(g id prop)` | Get a node property |
| [`set-node-property`](api/property-graph.md#set-node-property) | `(g id prop value)` | Set a node property |
| [`remove-node-property`](api/property-graph.md#remove-node-property) | `(g id prop)` | Remove a node property |
| [`node-properties`](api/property-graph.md#node-properties) | `(g id)` | All properties as alist |
| [`node-labels`](api/property-graph.md#node-labels) | `(g id)` | All labels |
| [`find-nodes`](api/property-graph.md#find-nodes) | `(g &key label)` | Find nodes by label |
| [`add-edge`](api/property-graph.md#add-edge) | `(g from to type &key properties)` | Create a typed edge |
| [`remove-edge`](api/property-graph.md#remove-edge) | `(g from to type)` | Remove an edge |
| [`get-edges`](api/property-graph.md#get-edges) | `(g &key from to type)` | Query edges |
| [`edge-from`](api/property-graph.md#edge-from) | `(edge)` | Edge source |
| [`edge-to`](api/property-graph.md#edge-to) | `(edge)` | Edge target |
| [`edge-type`](api/property-graph.md#edge-type) | `(edge)` | Edge type |
| [`edge-property`](api/property-graph.md#edge-property) | `(g edge prop)` | Get an edge property |
| [`neighbors`](api/property-graph.md#neighbors) | `(g id &key direction type)` | Get neighbor nodes |

## [Traversal](api/traversal.md)

| Function | Signature | Description |
|----------|-----------|-------------|
| [`traverse`](api/traversal.md#traverse) | `(g start &rest steps)` | Chainable graph traversal |
| [`traverse-with-path`](api/traversal.md#traverse-with-path) | `(g start &rest steps)` | Traversal returning full paths |
| [`traverse-depth`](api/traversal.md#traverse-depth) | `(g start pred &key max-depth)` | Depth-limited traversal |
| [`shortest-path`](api/traversal.md#shortest-path) | `(g from to &key edge-type)` | BFS shortest path |

## [Named Graphs](api/named-graphs.md)

| Function | Signature | Description |
|----------|-----------|-------------|
| [`add-quad`](api/named-graphs.md#add-quad) | `(g s p o graph-name)` | Add triple with graph name |
| [`get-quads`](api/named-graphs.md#get-quads) | `(g &key subject predicate object graph)` | Query quads |
| [`named-graphs`](api/named-graphs.md#named-graphs) | `(g)` | List all named graph URIs |

## [Transactions](api/transactions.md)

| Function | Signature | Description |
|----------|-----------|-------------|
| [`with-transaction`](api/transactions.md#with-transaction) | `((graph) &body body)` | Macro: auto-rollback on error |
| [`begin-transaction`](api/transactions.md#begin-transaction) | `(g)` | Start a transaction |
| [`rollback-transaction`](api/transactions.md#rollback-transaction) | `(tx)` | Rollback to snapshot |

## [Import / Export](api/import-export.md)

| Function | Signature | Description |
|----------|-----------|-------------|
| [`import-ntriples`](api/import-export.md#import-ntriples) | `(g string)` | Import N-Triples from string |
| [`import-ntriples-file`](api/import-export.md#import-ntriples-file) | `(g path)` | Import N-Triples from file |
| [`export-ntriples`](api/import-export.md#export-ntriples) | `(g)` | Export as N-Triples string |
| [`import-turtle`](api/import-export.md#import-turtle) | `(g string)` | Import Turtle from string |
| [`export-turtle`](api/import-export.md#export-turtle) | `(g)` | Export as Turtle string |
| [`import-nquads`](api/import-export.md#import-nquads) | `(g string)` | Import N-Quads from string |
| [`export-nquads`](api/import-export.md#export-nquads) | `(g)` | Export as N-Quads string |
| [`stream-import-ntriples`](api/import-export.md#stream-import-ntriples) | `(g path)` | Stream N-Triples from file |
| [`stream-import-nquads`](api/import-export.md#stream-import-nquads) | `(g path)` | Stream N-Quads from file |

## [Persistence](api/persistence.md)

| Function | Signature | Description |
|----------|-----------|-------------|
| [`save-graph`](api/persistence.md#save-graph) | `(g path)` | Save graph to file |
| [`load-graph`](api/persistence.md#load-graph) | `(path)` | Load graph from file |

## [Inference](api/inference.md)

| Function | Signature | Description |
|----------|-----------|-------------|
| [`defrule`](api/inference.md#defrule) | `(g name &key when then)` | Define an inference rule |
| [`remove-rule`](api/inference.md#remove-rule) | `(g name)` | Remove a rule by name |
| [`apply-rules`](api/inference.md#apply-rules) | `(g)` | Apply all rules to fixed point |
| [`graph-rules`](api/inference.md#graph-rules) | `(g)` | List all defined rules |

## [Reactive Triggers](api/reactive-triggers.md)

| Function | Signature | Description |
|----------|-----------|-------------|
| [`on-match`](api/reactive-triggers.md#on-match) | `(g name &key pattern callback)` | Register a trigger |
| [`remove-trigger`](api/reactive-triggers.md#remove-trigger) | `(g name)` | Remove a trigger |
| [`graph-triggers`](api/reactive-triggers.md#graph-triggers) | `(g)` | List all triggers |

## [Graph Analytics](api/graph-analytics.md)

| Function | Signature | Description |
|----------|-----------|-------------|
| [`pagerank`](api/graph-analytics.md#pagerank) | `(g &key predicate iterations damping)` | PageRank scores |
| [`connected-components`](api/graph-analytics.md#connected-components) | `(g &key predicate)` | Find connected components |
| [`degree-centrality`](api/graph-analytics.md#degree-centrality) | `(g &key predicate)` | Degree centrality scores |
| [`clustering-coefficient`](api/graph-analytics.md#clustering-coefficient) | `(g &key predicate)` | Clustering coefficients |

## [Graph Operations](api/graph-operations.md)

| Function | Signature | Description |
|----------|-----------|-------------|
| [`merge-graphs`](api/graph-operations.md#merge-graphs) | `(g1 g2)` | Create new merged graph |
| [`merge-graphs-into`](api/graph-operations.md#merge-graphs-into) | `(target source)` | Merge source into target |
| [`diff-graphs`](api/graph-operations.md#diff-graphs) | `(g1 g2)` | Triples in g1 not in g2 |
| [`copy-graph`](api/graph-operations.md#copy-graph) | `(g)` | Independent deep copy |

## [Visualization & REPL](api/visualization.md)

| Function | Signature | Description |
|----------|-----------|-------------|
| [`export-dot`](api/visualization.md#export-dot) | `(g &key predicates center depth file)` | Export as DOT/Graphviz |
| [`visualize-graph`](api/visualization.md#visualize-graph) | `(g &key file engine predicates center depth open)` | Render via Graphviz |
| [`describe-graph`](api/visualization.md#describe-graph) | `(g)` | Text summary of graph contents |
| [`format-results`](api/visualization.md#format-results) | `(rows headers)` | Format query results as table |

## [SPARQL String Parser](api/sparql-parser.md)

| Function | Signature | Description |
|----------|-----------|-------------|
| [`sparql`](api/sparql-parser.md#sparql) | `(g query-string)` | Parse and execute SPARQL string |
