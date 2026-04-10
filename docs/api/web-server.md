# Web Visualization

Hunchentoot-based web server with Cytoscape.js graph explorer.

## `start-web-server`

```lisp
(start-web-server graph &key port)
```

Start a web server serving an interactive graph visualization. Default port is 8080. Provides:

- Interactive Cytoscape.js graph explorer
- Predicate filter checkboxes
- Node search
- Layout switching (cose, circle, breadthfirst, grid)
- JSON API: `/api/graph`, `/api/info`, `/api/predicates`
- SPARQL endpoint: `/sparql?query=...`
- SPARQL UPDATE: `/update?update=...`

### Example

```lisp
(let ((g (make-graph :name "demo")))
  (add-triple g "alice" "knows" "bob")
  (start-web-server g :port 9090))
;; Open http://localhost:9090 in browser
```

## `stop-web-server`

```lisp
(stop-web-server)
```

Stop the running web server.
