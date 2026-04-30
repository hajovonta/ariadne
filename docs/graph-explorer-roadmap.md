# Ariadne Graph Explorer — Roadmap

## Current Status

Single-page Cytoscape.js graph viewer served by Hunchentoot. Dark theme, interactive exploration.

### What works
- Force-directed, hierarchical, circular, grid, concentric layouts
- Predicate filtering (checkbox panel)
- Node search with highlight/dim
- Click node: fetch full details (properties, incoming/outgoing) from API
- Hover: show node label (configurable: all / hover / none)
- Edge labels toggle (short local names, offset above arrows)
- Color coding: nodes by rdf:type with auto-assigned palette
- Legend panel showing type→color mapping
- Double-click node: recenter graph around it (2 hops)
- Right-click context menu: Expand, Collapse, Hide, Pin/Unpin
- Shift+box select: apply context menu operations to multiple nodes
- Reset button to restore full graph view
- Literal filtering: RDF literals shown in details panel, not as graph nodes
- rdfs:label used as display label when available
- Fit/zoom, node+edge count stats
- SPARQL query endpoint (`/sparql`, `/update`)
- API endpoints: `/api/graph`, `/api/predicates`, `/api/types`, `/api/node`

## Phase 1 — Visibility & Navigation ✓

- [x] Always-visible labels for nodes (configurable: all / on-hover / none)
- [x] Edge labels visible (toggle on/off)
- [x] Color coding: nodes by rdf:type, edges by predicate
- [x] Legend panel showing type→color mapping
- [x] Navigate to node by URI (double-click to recenter)
- [x] Reset view (Reset button)

## Phase 2 — Node Details & Exploration ✓

- [x] Node details panel: all properties, types, incoming/outgoing edges
- [x] Click-to-expand: load neighborhood of a node on demand
- [x] Collapse: hide expanded neighborhood (leaf nodes)
- [x] Double-click to recenter on a node
- [x] Right-click context menu (expand, collapse, hide, pin)

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
