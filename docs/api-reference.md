# Ariadne API Reference

Complete function reference for the Ariadne graph database.

---

## Graph

| Function | Signature | Description |
|----------|-----------|-------------|
| `make-graph` | `(&key name)` | Create a new empty graph |
| `graphp` | `(x)` | Test if x is a graph |
| `graph-name` | `(graph)` | Get graph name |
| `triple-count` | `(graph)` | Number of triples |
| `clear-graph` | `(graph)` | Remove all triples |

## Triples

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

## Query DSL

| Function | Signature | Description |
|----------|-----------|-------------|
| `query` | `(g expr)` | Execute a query expression |

## Pattern Matching

| Function | Signature | Description |
|----------|-----------|-------------|
| `variable-p` | `(x)` | Test if x is a `?variable` |
| `lookup-binding` | `(var env)` | Look up variable in bindings |
| `unify` | `(pattern value env)` | Unify pattern with value |
| `match-pattern` | `(g pattern)` | Match single triple pattern |
| `match-patterns` | `(g patterns)` | Match and join multiple patterns |

## Property Graph

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

## Traversal

| Function | Signature | Description |
|----------|-----------|-------------|
| `traverse` | `(g start &rest steps)` | Chainable graph traversal |
| `traverse-with-path` | `(g start &rest steps)` | Traversal returning full paths |
| `traverse-depth` | `(g start pred &key max-depth)` | Depth-limited traversal |
| `shortest-path` | `(g from to &key edge-type)` | BFS shortest path |

## Named Graphs

| Function | Signature | Description |
|----------|-----------|-------------|
| `add-quad` | `(g s p o graph-name)` | Add triple with graph name |
| `get-quads` | `(g &key subject predicate object graph)` | Query quads |
| `named-graphs` | `(g)` | List all named graph URIs |

## Transactions

| Function | Signature | Description |
|----------|-----------|-------------|
| `with-transaction` | `((graph) &body body)` | Macro: auto-rollback on error |
| `begin-transaction` | `(g)` | Start a transaction |
| `rollback-transaction` | `(tx)` | Rollback to snapshot |

## Import / Export

| Function | Signature | Description |
|----------|-----------|-------------|
| `import-ntriples` | `(g string)` | Import N-Triples from string |
| `import-ntriples-file` | `(g path)` | Import N-Triples from file |
| `export-ntriples` | `(g)` | Export as N-Triples string |
| `import-turtle` | `(g string)` | Import Turtle from string |
| `export-turtle` | `(g)` | Export as Turtle string |
| `import-nquads` | `(g string)` | Import N-Quads from string |
| `export-nquads` | `(g)` | Export as N-Quads string |
| `stream-import-ntriples` | `(g path)` | Stream N-Triples from file |
| `stream-import-nquads` | `(g path)` | Stream N-Quads from file |

## Persistence

| Function | Signature | Description |
|----------|-----------|-------------|
| `save-graph` | `(g path)` | Save graph to file |
| `load-graph` | `(path)` | Load graph from file |

## Inference

| Function | Signature | Description |
|----------|-----------|-------------|
| `defrule` | `(g name &key when then)` | Define an inference rule |
| `remove-rule` | `(g name)` | Remove a rule by name |
| `apply-rules` | `(g)` | Apply all rules to fixed point |
| `graph-rules` | `(g)` | List all defined rules |

## Reactive Triggers

| Function | Signature | Description |
|----------|-----------|-------------|
| `on-match` | `(g name &key pattern callback)` | Register a trigger |
| `remove-trigger` | `(g name)` | Remove a trigger |
| `graph-triggers` | `(g)` | List all triggers |

## Graph Analytics

| Function | Signature | Description |
|----------|-----------|-------------|
| `pagerank` | `(g &key predicate iterations damping)` | PageRank scores |
| `connected-components` | `(g &key predicate)` | Find connected components |
| `degree-centrality` | `(g &key predicate)` | Degree centrality scores |
| `clustering-coefficient` | `(g &key predicate)` | Clustering coefficients |

## Graph Operations

| Function | Signature | Description |
|----------|-----------|-------------|
| `merge-graphs` | `(g1 g2)` | Create new merged graph |
| `merge-graphs-into` | `(target source)` | Merge source into target |
| `diff-graphs` | `(g1 g2)` | Triples in g1 not in g2 |
| `copy-graph` | `(g)` | Independent deep copy |

## Visualization & REPL

| Function | Signature | Description |
|----------|-----------|-------------|
| `export-dot` | `(g &key predicates center depth file)` | Export as DOT/Graphviz |
| `visualize-graph` | `(g &key file engine predicates center depth open)` | Render via Graphviz |
| `describe-graph` | `(g)` | Text summary of graph contents |
| `format-results` | `(rows headers)` | Format query results as table |

## SPARQL String Parser

| Function | Signature | Description |
|----------|-----------|-------------|
| `sparql` | `(g query-string)` | Parse and execute SPARQL string |
