# Ariadne Graph Explorer — Roadmap

## Current Status

Single-page Cytoscape.js graph viewer served by Hunchentoot. Dark theme, basic interactivity. All HTML/CSS/JS in one inline Lisp string.

### What works
- Force-directed, hierarchical, circular, grid, concentric layouts
- Predicate filtering (checkbox panel)
- Node search with highlight/dim
- Click node: highlight neighborhood, show connections in info panel
- Hover: show node label
- Fit/zoom, node+edge count stats
- SPARQL query endpoint (`/sparql`, `/update`)

## Phase 1 — Visibility & Navigation

- [ ] Always-visible labels for nodes (configurable: all / on-hover / none)
- [ ] Edge labels visible (toggle on/off)
- [ ] Color coding: nodes by rdf:type, edges by predicate
- [ ] Legend panel showing type→color mapping
- [ ] Navigate to node by URI (URL parameter or search-and-center)
- [ ] Breadcrumb / back navigation (history of focused nodes)

## Phase 2 — Node Details & Exploration

- [ ] Node details panel: all properties, types, incoming/outgoing edges
- [ ] Click-to-expand: load neighborhood of a node on demand (not all at once)
- [ ] Collapse: hide expanded neighborhood
- [ ] Double-click to recenter on a node
- [ ] Right-click context menu (expand, collapse, hide, pin)

## Phase 3 — Query Integration

- [ ] SPARQL query box in the UI with results table
- [ ] Query results highlight matching nodes in the graph
- [ ] "Show in graph" button for query result rows
- [ ] Query history / saved queries

## Phase 4 — Large Graph Support

- [ ] Pagination: load subgraph around focus node, expand on demand
- [ ] Node count limit with "load more" affordance
- [ ] Server-side filtering (don't send full graph to browser)
- [ ] Performance: virtual rendering for 1000+ nodes

## Phase 5 — Export & Sharing

- [ ] Export visible graph as PNG/SVG
- [ ] Export current view as SPARQL CONSTRUCT
- [ ] Shareable URL (encodes focus node, predicates, layout)
- [ ] Embed mode (iframe-friendly, no toolbar)

## Phase 6 — Polish & Maintainability

- [ ] Extract HTML/CSS/JS to static files (not inline string)
- [ ] Responsive layout for smaller screens
- [ ] Keyboard shortcuts (search, fit, undo)
- [ ] Accessibility: ARIA labels, keyboard navigation
- [ ] Graph statistics panel (triple count, top predicates, type distribution)
- [ ] Dark/light theme toggle
