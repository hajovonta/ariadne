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

## Phase 3 — Query Integration ✓

- [x] SPARQL query box in the UI with results table
- [x] Query results highlight matching nodes in the graph
- [x] "Show in graph" button for query result rows (loads node if not in view)
- [x] Live CONSTRUCT query display (auto-updates with filters)
- [x] Run CONSTRUCT directly from the bar

## Phase 4 — Large Graph Support ✓

- [x] Auto-truncate to 200 nodes max on initial load (BFS from highest-degree node)
- [x] Node count shows "X of Y nodes" when truncated
- [x] Server-side type filtering (Types panel triggers re-query)
- [x] Server-side predicate filtering
- [x] Double-click to explore beyond boundary
- [x] Click SPARQL result loads node into view if not present

## Phase 5 — Export & Sharing ✓

- [x] Export visible graph as PNG
- [x] Export visible graph as SVG
- [x] Live CONSTRUCT query (copy or run)
- [x] Shareable URL (encodes focus node, predicates, layout)
- [ ] Embed mode (iframe-friendly, no toolbar)

## Phase 6 — Polish & Maintainability ✓

- [x] Extract HTML/CSS/JS to static file (static/index.html)
- [x] Responsive toolbar (wraps on narrow windows)
- [x] Keyboard shortcuts (/ search, f fit, r reset, Escape clear)
- [x] Graph statistics panel (click node count for details)
- [x] Dark/light theme toggle with adaptive colors
- [x] All/None buttons in filter panels
- [ ] Accessibility: ARIA labels, keyboard navigation (low priority)
