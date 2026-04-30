;;;; web-server.lisp
;;;; Interactive graph visualization with Hunchentoot + Cytoscape.js

(in-package #:ariadne)

(defvar *web-server* nil)
(defvar *web-graph* nil)

;;; ==========================================================================
;;; JSON conversion
;;; ==========================================================================

(defun graph-to-cytoscape-json (g &key predicates center depth (max-nodes 200) node-types)
  "Convert graph to Cytoscape.js elements JSON string."
  (let ((triples (if (or predicates center)
                     (let ((trs (get-triples g)))
                       (when predicates
                         (setf trs (remove-if-not
                                    (lambda (tr)
                                      (member (triple-predicate tr) predicates
                                              :test #'equal))
                                    trs)))
                       (when (and center depth)
                         (let ((reachable (make-hash-table :test 'equal)))
                           (labels ((walk (node d)
                                      (when (and (>= d 0)
                                                 (not (gethash node reachable))
                                                 (or (not max-nodes) (< (hash-table-count reachable) max-nodes)))
                                        (setf (gethash node reachable) t)
                                        (when (> d 0)
                                          (dolist (tr trs)
                                            (when (equal (triple-subject tr) node)
                                              (walk (triple-object tr) (1- d)))
                                            (when (equal (triple-object tr) node)
                                              (walk (triple-subject tr) (1- d))))))))
                             (walk center depth))
                           (setf trs (remove-if-not
                                      (lambda (tr)
                                        (and (gethash (triple-subject tr) reachable)
                                             (gethash (triple-object tr) reachable)))
                                      trs))))
                       trs)
                     (get-triples g)))
        (nodes (make-hash-table :test 'equal))
        (edges nil))
    ;; Filter by node types if specified
    (when node-types
      (let ((typed-nodes (make-hash-table :test 'equal)))
        (dolist (tr (append (get-triples g :predicate "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                            (get-triples g :predicate "rdf:type")))
          (when (member (princ-to-string (triple-object tr)) node-types :test #'equal)
            (setf (gethash (princ-to-string (triple-subject tr)) typed-nodes) t)))
        (setf triples (remove-if-not
                        (lambda (tr)
                          (or (gethash (princ-to-string (triple-subject tr)) typed-nodes)
                              (gethash (princ-to-string (triple-object tr)) typed-nodes)))
                        triples))))
    ;; Collect nodes and edges (skip literal objects)
    (dolist (tr triples)
      (let* ((subj (princ-to-string (triple-subject tr)))
             (obj-raw (triple-object tr))
             (obj (princ-to-string obj-raw))
             (obj-is-resource (not (typep obj-raw 'rdf-literal))))
        (setf (gethash subj nodes) t)
        (when obj-is-resource
          (setf (gethash obj nodes) t)
          (push tr edges))))
    ;; Truncate if too many nodes and no center specified
    (let ((truncated nil)
          (total-nodes (hash-table-count nodes)))
      (when (and max-nodes (> total-nodes max-nodes) (not center))
        (setf truncated total-nodes)
        ;; Find highest-degree node
        (let ((degree (make-hash-table :test 'equal)))
          (dolist (tr edges)
            (incf (gethash (princ-to-string (triple-subject tr)) degree 0))
            (incf (gethash (princ-to-string (triple-object tr)) degree 0)))
          (let ((best nil) (best-deg 0))
            (maphash (lambda (k v) (when (> v best-deg) (setf best k best-deg v))) degree)
            ;; BFS from best node up to max-nodes
            (let ((keep (make-hash-table :test 'equal))
                  (queue (list best)))
              (setf (gethash best keep) t)
              (loop while (and queue (< (hash-table-count keep) max-nodes))
                    do (let ((cur (pop queue)))
                         (dolist (tr edges)
                           (let ((s (princ-to-string (triple-subject tr)))
                                 (o (princ-to-string (triple-object tr))))
                             (when (and (equal s cur) (not (gethash o keep))
                                        (< (hash-table-count keep) max-nodes))
                               (setf (gethash o keep) t)
                               (push o queue))
                             (when (and (equal o cur) (not (gethash s keep))
                                        (< (hash-table-count keep) max-nodes))
                               (setf (gethash s keep) t)
                               (push s queue))))))
              ;; Filter nodes and edges
              (let ((new-nodes (make-hash-table :test 'equal))
                    (new-edges nil))
                (maphash (lambda (k v) (declare (ignore v))
                           (when (gethash k keep) (setf (gethash k new-nodes) t))) nodes)
                (dolist (tr edges)
                  (when (and (gethash (princ-to-string (triple-subject tr)) keep)
                             (gethash (princ-to-string (triple-object tr)) keep))
                    (push tr new-edges)))
                (setf nodes new-nodes edges new-edges))))))
    ;; Build JSON
    (let ((elements nil)
          (id-map (make-hash-table :test 'equal))
          (id-counter 0))
      ;; Assign safe IDs
      (maphash (lambda (uri v)
                 (declare (ignore v))
                 (setf (gethash uri id-map) (format nil "n~A" (incf id-counter))))
               nodes)
      ;; Build node type and label lookups
      (let ((node-types (make-hash-table :test 'equal))
            (node-labels (make-hash-table :test 'equal)))
        (dolist (tr (append (get-triples g :predicate "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                            (get-triples g :predicate "rdf:type")))
          (setf (gethash (princ-to-string (triple-subject tr)) node-types)
                (princ-to-string (triple-object tr))))
        (dolist (tr (append (get-triples g :predicate "http://www.w3.org/2000/01/rdf-schema#label")
                            (get-triples g :predicate "rdfs:label")))
          (setf (gethash (princ-to-string (triple-subject tr)) node-labels)
                (princ-to-string (triple-object tr))))
        ;; Nodes
        (maphash (lambda (uri v)
                   (declare (ignore v))
                   (let ((data (make-hash-table :test 'equal)))
                     (setf (gethash "id" data) (gethash uri id-map))
                     (setf (gethash "uri" data) uri)
                     (setf (gethash "label" data) (or (gethash uri node-labels) (node-label uri)))
                     (let ((typ (gethash uri node-types)))
                       (when typ (setf (gethash "type" data) typ)))
                     (let ((el (make-hash-table :test 'equal)))
                       (setf (gethash "data" el) data)
                       (push el elements))))
                 nodes))
      ;; Edges
      (let ((eid 0))
        (dolist (tr edges)
          (let* ((src (princ-to-string (triple-subject tr)))
                 (tgt (princ-to-string (triple-object tr)))
                 (data (make-hash-table :test 'equal)))
            (setf (gethash "id" data) (format nil "e~A" (incf eid)))
            (setf (gethash "source" data) (gethash src id-map))
            (setf (gethash "target" data) (gethash tgt id-map))
            (setf (gethash "label" data) (triple-predicate tr))
            (let ((el (make-hash-table :test 'equal)))
              (setf (gethash "data" el) data)
              (push el elements)))))
      (let ((result (make-hash-table :test 'equal)))
        (setf (gethash "elements" result) (coerce (nreverse elements) 'vector))
        (setf (gethash "totalNodes" result) total-nodes)
        (jzon:stringify result))))))

(defun node-label (id)
  "Short label for a node: strip URI prefix."
  (let ((s (princ-to-string id)))
    (or (let ((hash (position #\# s :from-end t)))
          (when hash (subseq s (1+ hash))))
        (let ((slash (position #\/ s :from-end t)))
          (when (and slash (> slash 8)) (subseq s (1+ slash))))
        s)))

(defvar *type-palette*
  '("#e94560" "#0f3460" "#ffd700" "#00d2d3" "#ff9f43"
    "#ee5a24" "#6ab04c" "#be2edd" "#22a6b3" "#f368e0"
    "#c44569" "#574b90" "#f78fb3" "#3dc1d3" "#e77f67"))

(defun graph-types-json (g)
  "Return JSON mapping rdf:type values to colors."
  (let ((types (mapcar #'triple-object
                       (append (get-triples g :predicate "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                               (get-triples g :predicate "rdf:type"))))
        (type-map (make-hash-table :test 'equal)))
    (let ((unique (remove-duplicates types :test #'equal))
          (i 0))
      (dolist (typ unique)
        (setf (gethash (princ-to-string typ) type-map)
              (nth (mod i (length *type-palette*)) *type-palette*))
        (incf i)))
    (jzon:stringify type-map)))

(defun node-info-json (g node-id)
  "Return JSON with outgoing and incoming triples for NODE-ID."
  (let ((outgoing (get-triples g :subject node-id))
        (incoming (get-triples g :object node-id))
        (result (make-hash-table :test 'equal)))
    (setf (gethash "id" result) node-id)
    (setf (gethash "label" result) (node-label node-id))
    (setf (gethash "outgoing" result)
          (coerce (mapcar (lambda (tr)
                            (let ((ht (make-hash-table :test 'equal)))
                              (setf (gethash "predicate" ht) (princ-to-string (triple-predicate tr)))
                              (setf (gethash "object" ht) (princ-to-string (triple-object tr)))
                              ht))
                          outgoing) 'vector))
    (setf (gethash "incoming" result)
          (coerce (mapcar (lambda (tr)
                            (let ((ht (make-hash-table :test 'equal)))
                              (setf (gethash "predicate" ht) (princ-to-string (triple-predicate tr)))
                              (setf (gethash "subject" ht) (princ-to-string (triple-subject tr)))
                              ht))
                          incoming) 'vector))
    (jzon:stringify result)))

;;; ==========================================================================
;;; HTML page
;;; ==========================================================================

(defun graph-page-html ()
  "Return the HTML page with Cytoscape.js graph viewer."
  (format nil "<!DOCTYPE html>
<html><head>
<title>Ariadne Graph Explorer</title>
<script src='https://unpkg.com/cytoscape@3.28.1/dist/cytoscape.min.js'></script>
<script src='https://unpkg.com/layout-base@2.0.1/layout-base.js'></script>
<script src='https://unpkg.com/cose-base@2.2.0/cose-base.js'></script>
<script src='https://unpkg.com/cytoscape-cose-bilkent@4.1.0/cytoscape-cose-bilkent.js'></script>
<script src='https://unpkg.com/dagre@0.8.5/dist/dagre.min.js'></script>
<script src='https://unpkg.com/cytoscape-dagre@2.5.0/cytoscape-dagre.js'></script>
<script src='https://unpkg.com/cytoscape-svg@0.4.0/cytoscape-svg.js'></script>
<style>
  body { margin: 0; font-family: sans-serif; background: #1a1a2e; color: #eee; }
  #cy { width: 100%%; height: calc(100vh - 50px - 36px); }
  #toolbar { height: 50px; display: flex; align-items: center; padding: 0 16px; gap: 12px; background: #16213e; flex-wrap: wrap; }
  #toolbar input, #toolbar select, #toolbar button {
    padding: 6px 10px; border-radius: 4px; border: 1px solid #444; background: #0f3460; color: #eee; }
  #toolbar button { cursor: pointer; }
  #toolbar button:hover { background: #e94560; }
  #info { position: fixed; bottom: 16px; right: 16px; background: #16213e; padding: 12px;
    border-radius: 8px; max-width: 350px; max-height: 30vh; overflow-y: auto;
    font-size: 13px; display: none; border: 1px solid #333; z-index: 60; }
  #predicates { max-width: 300px; }
  #pred-panel { position: fixed; top: 50px; left: 0; background: #16213e; padding: 12px;
    border-right: 1px solid #333; border-bottom: 1px solid #333; border-radius: 0 0 8px 0;
    max-height: 80vh; overflow-y: auto; display: none; min-width: 200px; z-index: 10; }
  #pred-panel label { display: block; padding: 3px 0; cursor: pointer; font-size: 13px; }
  #pred-panel label:hover { color: #e94560; }
  #pred-panel input { margin-right: 6px; }
  #type-panel { position: fixed; top: 50px; left: 220px; background: #16213e; padding: 12px;
    border-right: 1px solid #333; border-bottom: 1px solid #333; border-radius: 0 0 8px 0;
    max-height: 80vh; overflow-y: auto; display: none; min-width: 180px; z-index: 10; }
  #type-panel label { display: block; padding: 3px 0; cursor: pointer; font-size: 13px; }
  #type-panel label:hover { color: #e94560; }
  #type-panel input { margin-right: 6px; }
  #legend { position: fixed; top: 50px; right: 0; background: #16213e; padding: 12px;
    border-left: 1px solid #333; border-bottom: 1px solid #333; border-radius: 0 0 0 8px;
    font-size: 12px; display: none; z-index: 10; }
  .legend-item { padding: 2px 0; display: flex; align-items: center; gap: 6px; }
  .legend-swatch { width: 12px; height: 12px; border-radius: 50%%; display: inline-block; }
  #ctx-menu { position: fixed; background: #16213e; border: 1px solid #444; border-radius: 6px;
    padding: 4px 0; display: none; z-index: 100; min-width: 140px; }
  #ctx-menu div { padding: 6px 14px; cursor: pointer; font-size: 13px; }
  #ctx-menu div:hover { background: #e94560; }
  #query-panel { position: fixed; bottom: 0; left: 0; right: 0; background: #16213e;
    border-top: 1px solid #444; z-index: 50; max-height: 40vh; overflow: hidden;
    display: none; flex-direction: column; }
  #query-panel.open { display: flex; }
  #query-input { width: 100%%; height: 60px; background: #0f3460; color: #eee; border: none;
    padding: 8px; font-family: monospace; font-size: 13px; resize: none; }
  #query-bar { display: flex; gap: 8px; padding: 6px 8px; align-items: center; }
  #query-bar button { padding: 4px 12px; border-radius: 4px; border: 1px solid #444;
    background: #0f3460; color: #eee; cursor: pointer; }
  #query-bar button:hover { background: #e94560; }
  #query-results { overflow: auto; flex: 1; padding: 0 8px 8px; }
  #query-results table { width: 100%%; border-collapse: collapse; font-size: 12px; }
  #query-results th { text-align: left; padding: 4px 8px; border-bottom: 1px solid #444; color: #aaa; }
  #query-results td { padding: 4px 8px; border-bottom: 1px solid #333; cursor: pointer; }
  #query-results td:hover { color: #ffd700; }
  #construct-panel { position: fixed; bottom: 0; left: 0; right: 0; background: #0f3460;
    border-top: 1px solid #444; font-family: monospace; font-size: 11px; color: #aaa;
    padding: 6px 12px; white-space: pre-wrap; max-height: 80px; overflow-y: auto;
    display: flex; align-items: flex-start; gap: 8px; z-index: 40; }
  #construct-panel code { flex: 1; overflow-x: auto; }
  #construct-panel button { padding: 2px 8px; border-radius: 4px; border: 1px solid #444;
    background: #16213e; color: #eee; cursor: pointer; font-size: 11px; white-space: nowrap; }
  #construct-panel button:hover { background: #e94560; }
</style>
</head><body>
<div id='toolbar'>
  <strong>Ariadne</strong>
  <input id='search' placeholder='Search nodes...' oninput='searchNodes()'>
  <select id='predicates' multiple title='Filter predicates (ctrl+click)' style='display:none'></select>
  <button onclick='togglePredPanel()'>Predicates ▼</button>
  <button onclick='toggleTypePanel()'>Types ▼</button>
  <button onclick='loadGraph()'>Apply</button>
  <button onclick='resetGraph()'>Reset</button>
  <select id='layout' onchange='changeLayout()'>
    <option value='cose-bilkent'>Force (bilkent)</option>
    <option value='dagre'>Dagre (DAG)</option>
    <option value='cose'>Force (basic)</option>
    <option value='breadthfirst'>Hierarchical</option>
    <option value='circle'>Circular</option>
    <option value='concentric'>Concentric</option>
    <option value='grid'>Grid</option>
  </select>
  <select id='labelMode' onchange='updateLabelMode()'>
    <option value='hover'>Labels: hover</option>
    <option value='all'>Labels: all</option>
    <option value='none'>Labels: none</option>
  </select>
  <label><input type='checkbox' id='edgeLabel' onchange='updateEdgeLabels()'> Edge labels</label>
  <input type='range' id='spacing' min='1' max='10' value='5' title='Node spacing' onchange='changeLayout()'>
  <button onclick='cy.fit()'>Fit</button>
  <button onclick='selectAll()'>All predicates</button>
  <button onclick='toggleQueryPanel()'>SPARQL</button>
  <span id='stats'></span>
</div>
<div id='cy'></div>
<div id='pred-panel'></div>
<div id='type-panel'></div>
<div id='legend'></div>
<div id='info'></div>
<div id='ctx-menu'>
  <div onclick='ctxExpand()'>Expand</div>
  <div onclick='ctxCollapse()'>Collapse</div>
  <div onclick='ctxHide()'>Hide</div>
  <div onclick='ctxPin()'>Pin/Unpin</div>
</div>
<div id='query-panel'>
  <div id='query-bar'>
    <button onclick='runQuery()'>Run</button>
    <button onclick='toggleQueryPanel()'>Close</button>
    <span id='query-status'></span>
  </div>
  <textarea id='query-input' placeholder='SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10'></textarea>
  <div id='query-results'></div>
</div>
<div id='construct-panel'>
  <code id='construct-query'></code>
  <button onclick='copyConstruct()'>Copy</button>
  <button onclick='runConstruct()'>Run</button>
  <button onclick='exportPNG()'>PNG</button>
  <button onclick='exportSVG()'>SVG</button>
</div>
<script>
let cy;
let typeColors = {};
// Load predicate list and type colors
Promise.all([
  fetch('/api/predicates').then(r=>r.json()),
  fetch('/api/types').then(r=>r.json())
]).then(([preds, types]) => {
  typeColors = types;
  buildLegend(types);
  // Build type filter panel
  let typePanel = document.getElementById('type-panel');
  // Get counts
  fetch('/api/type-counts').then(r=>r.json()).then(counts=>{
    for(let [typ, color] of Object.entries(types)){
      let label = document.createElement('label');
      let cb = document.createElement('input');
      cb.type = 'checkbox'; cb.value = typ;
      cb.checked = true;
      cb.onchange = applyTypeFilter;
      label.appendChild(cb);
      let swatch = document.createElement('span');
      swatch.className = 'legend-swatch';
      swatch.style.background = color;
      label.appendChild(swatch);
      let short = typ.split('#').pop().split('/').pop();
      let count = counts[typ] || 0;
      label.appendChild(document.createTextNode(' ' + short + ' (' + count + ')'));
      typePanel.appendChild(label);
    }
  });
  document.getElementById('cy').addEventListener('contextmenu', e => e.preventDefault());
  document.addEventListener('click', hideCtxMenu);
  let panel = document.getElementById('pred-panel');
  preds.forEach((p,i) => {
    let label = document.createElement('label');
    let cb = document.createElement('input');
    cb.type = 'checkbox'; cb.value = p;
    cb.checked = i < 3;
    cb.onchange = loadGraph;
    label.appendChild(cb);
    label.appendChild(document.createTextNode(p.split('#').pop().split('/').pop()));
    panel.appendChild(label);
  });
  loadGraph();
});
function buildLegend(types){
  let legend = document.getElementById('legend');
  legend.innerHTML = '';
  for(let [typ, color] of Object.entries(types)){
    let item = document.createElement('div');
    item.className = 'legend-item';
    item.innerHTML = '<span class=\"legend-swatch\" style=\"background:'+color+'\"></span>'
      + typ.split('#').pop().split('/').pop();
    legend.appendChild(item);
  }
  if(Object.keys(types).length > 0) legend.style.display = 'block';
}
function getSelectedPredicates(){
  return Array.from(document.querySelectorAll('#pred-panel input:checked')).map(cb => cb.value);
}
function togglePredPanel(){
  let p = document.getElementById('pred-panel');
  p.style.display = p.style.display === 'none' ? 'block' : 'none';
  document.getElementById('type-panel').style.display = 'none';
}
function toggleTypePanel(){
  let p = document.getElementById('type-panel');
  p.style.display = p.style.display === 'none' ? 'block' : 'none';
  document.getElementById('pred-panel').style.display = 'none';
}
function applyTypeFilter(){
  loadGraph();
}
function loadGraph(){
  let selected = getSelectedPredicates();
  let total = document.querySelectorAll('#pred-panel input').length;
  let url = '/api/graph';
  let params = [];
  if(selected.length > 0 && selected.length < total)
    params.push('predicates=' + encodeURIComponent(selected.join(',')));
  let typeChecks = Array.from(document.querySelectorAll('#type-panel input:checked')).map(cb => cb.value);
  let typeTotal = document.querySelectorAll('#type-panel input').length;
  if(typeChecks.length > 0 && typeChecks.length < typeTotal)
    params.push('types=' + encodeURIComponent(typeChecks.join(',')));
  if(params.length > 0) url += '?' + params.join('&');
  fetch(url).then(r=>r.json()).then(resp=>{
    let data = resp.elements;
    let totalNodes = resp.totalNodes;
    if(cy) cy.destroy();
    let labelMode = document.getElementById('labelMode').value;
    let showEdgeLabels = document.getElementById('edgeLabel').checked;
    cy = cytoscape({
      container: document.getElementById('cy'),
      elements: data,
      style: [
        { selector: 'node', style: {
          'label': labelMode==='all' ? 'data(label)' : '',
          'background-color': '#e94560',
          'color': '#eee', 'font-size': '8px', 'min-zoomed-font-size': 8,
          'text-valign': 'bottom', 'text-margin-y': 4,
          'width': 14, 'height': 14 }},
        { selector: 'edge', style: {
          'curve-style': 'bezier',
          'label': '',
          'font-size': '7px', 'color': '#aaa', 'text-rotation': 'autorotate',
          'text-margin-y': -10, 'text-background-color': '#1a1a2e',
          'text-background-opacity': 0.8, 'text-background-padding': '2px',
          'target-arrow-shape': 'triangle', 'line-color': '#0f3460',
          'target-arrow-color': '#0f3460', 'width': 1.5, 'opacity': 0.6 }},
        { selector: 'node:selected', style: { 'background-color': '#ffd700', 'border-width': 3, 'border-color': '#ffd700' }},
        { selector: 'edge:selected', style: { 'line-color': '#ffd700', 'target-arrow-color': '#ffd700' }},
        { selector: 'node.highlighted', style: {
          'background-color': '#ffd700', 'label': 'data(label)', 'border-width': 3, 'border-color': '#fff',
          'color': '#eee', 'font-size': '11px', 'text-valign': 'bottom', 'text-margin-y': 4 }},
        { selector: 'edge.highlighted', style: {
          'line-color': '#ffd700', 'target-arrow-color': '#ffd700', 'opacity': 1 }},
        { selector: '.dimmed', style: { opacity: 0.08 }}
      ],
      layout: getLayoutOpts(false),
      wheelSensitivity: 0.15,
      minZoom: 0.1,
      maxZoom: 10
    });
    // Apply type colors via classes
    cy.nodes().forEach(n => {
      let t = n.data('type');
      if(t && typeColors[t]) n.addClass('type-' + t.replace(/[^a-zA-Z0-9]/g, '_'));
    });
    // Add type color styles
    let typeStyles = [];
    for(let [typ, color] of Object.entries(typeColors)){
      let cls = 'type-' + typ.replace(/[^a-zA-Z0-9]/g, '_');
      typeStyles.push({selector: 'node.' + cls, style: {'background-color': color}});
    }
    if(typeStyles.length > 0) cy.style().append(typeStyles).update();
    if(showEdgeLabels) updateEdgeLabels();
    document.getElementById('stats').textContent =
      cy.nodes().length + (totalNodes > cy.nodes().length ? ' of ' + totalNodes : '') +
      ' nodes, ' + cy.edges().length + ' edges';
    updateConstruct();
    cy.on('mouseover', 'node', function(e){
      if(document.getElementById('labelMode').value === 'hover'){
        e.target.style('label', e.target.data('label'));
      }
    });
    cy.on('mouseout', 'node', function(e){
      if(document.getElementById('labelMode').value === 'hover' && !e.target.hasClass('highlighted'))
        e.target.style('label', '');
    });
    cy.on('tap', 'node', function(e){
      let n = e.target;
      cy.elements().removeClass('highlighted dimmed');
      let hood = n.neighborhood().add(n);
      hood.addClass('highlighted');
      cy.elements().not(hood).addClass('dimmed');
      // Fetch full node details
      fetch('/api/node?id=' + encodeURIComponent(n.data('uri'))).then(r=>r.json()).then(details=>{
        let info = '<b>' + details.label + '</b><br>';
        if(details.outgoing && details.outgoing.length > 0){
          info += '<br><u>Properties</u><br>';
          details.outgoing.forEach(t => {
            let pred = t.predicate.split('#').pop().split('/').pop();
            let obj = t.object.split('#').pop().split('/').pop();
            info += '<i>' + pred + '</i>: ' + obj + '<br>';
          });
        }
        if(details.incoming && details.incoming.length > 0){
          info += '<br><u>Referenced by</u><br>';
          details.incoming.forEach(t => {
            let pred = t.predicate.split('#').pop().split('/').pop();
            let subj = t.subject.split('#').pop().split('/').pop();
            info += subj + ' <i>' + pred + '</i><br>';
          });
        }
        let el = document.getElementById('info');
        el.innerHTML = info; el.style.display = 'block';
      });
    });
    cy.on('tap', function(e){ if(e.target===cy){
      cy.elements().removeClass('highlighted dimmed');
      document.getElementById('info').style.display='none';
      hideCtxMenu();
    }});
    cy.on('zoom', function(){
      if(document.getElementById('labelMode').value === 'all') applyZoomLabels();
    });
    cy.on('cxttap', 'node', function(e){
      e.originalEvent.preventDefault();
      showCtxMenu(e.originalEvent.clientX, e.originalEvent.clientY, e.target);
    });
    cy.on('dblclick', 'node', function(e){
      let id = e.target.data('uri');
      let selected = getSelectedPredicates();
      let total = document.querySelectorAll('#pred-panel input').length;
      let url = '/api/graph?center=' + encodeURIComponent(id) + '&depth=2';
      if(selected.length > 0 && selected.length < total)
        url += '&predicates=' + encodeURIComponent(selected.join(','));
      fetch(url).then(r=>r.json()).then(resp=>{
        let data = resp.elements || resp;
        cy.elements().remove();
        cy.add(data);
        cy.nodes().forEach(n => {
          let t = n.data('type');
          if(t && typeColors[t]) n.style('background-color', typeColors[t]);
        });
        let mode = document.getElementById('labelMode').value;
        if(mode==='all') cy.nodes().forEach(n => n.style('label', n.data('label')));
        if(document.getElementById('edgeLabel').checked) updateEdgeLabels();
        cy.layout({ name: document.getElementById('layout').value, animate: true }).run();
        document.getElementById('stats').textContent =
          cy.nodes().length + ' nodes, ' + cy.edges().length + ' edges';
      });
    });
  });
}
function searchNodes(){
  let q = document.getElementById('search').value.toLowerCase();
  cy.elements().removeClass('highlighted dimmed');
  if(!q) return;
  let matches = cy.nodes().filter(n => n.data('label').toLowerCase().includes(q));
  if(matches.length > 0){
    matches.addClass('highlighted');
    cy.elements().not(matches).addClass('dimmed');
    cy.fit(matches, 50);
  }
}
function getLayoutOpts(animate){
  let name = document.getElementById('layout').value;
  let s = parseInt(document.getElementById('spacing').value);
  let opts = { name: name, animate: animate };
  if(name === 'cose-bilkent') { opts.nodeRepulsion = s * 4000; opts.idealEdgeLength = s * 24; opts.animate = animate ? 'end' : false; opts.gravityRange = 1.5; }
  if(name === 'cose') { opts.nodeRepulsion = function(){ return s * 8000; }; opts.idealEdgeLength = function(){ return s * 20; }; }
  if(name === 'dagre') { opts.rankDir = 'TB'; opts.nodeSep = s * 12; opts.rankSep = s * 20; }
  if(name === 'breadthfirst') { opts.spacingFactor = s * 0.3; }
  return opts;
}
function changeLayout(){
  cy.layout(getLayoutOpts(true)).run();
}
function selectAll(){
  document.querySelectorAll('#pred-panel input').forEach(cb => cb.checked = true);
  loadGraph();
}
function resetGraph(){
  document.querySelectorAll('#pred-panel input').forEach(cb => cb.checked = true);
  loadGraph();
}
let ctxNode = null;
function showCtxMenu(x, y, node){
  ctxNode = node;
  let menu = document.getElementById('ctx-menu');
  menu.style.left = x + 'px';
  menu.style.top = y + 'px';
  menu.style.display = 'block';
}
function hideCtxMenu(){ document.getElementById('ctx-menu').style.display = 'none'; }
function ctxExpand(){
  hideCtxMenu();
  if(!ctxNode) return;
  let id = ctxNode.data('uri');
  let cyId = ctxNode.id();
  fetch('/api/node?id=' + encodeURIComponent(id)).then(r=>r.json()).then(details=>{
    let existingUris = new Set(cy.nodes().map(n => n.data('uri')));
    let toAdd = [];
    let counter = cy.nodes().length;
    details.outgoing.forEach(t => {
      let tgtId;
      let existing = cy.nodes().filter(n => n.data('uri') === t.object);
      if(existing.length > 0){ tgtId = existing[0].id(); }
      else { tgtId = 'n' + (++counter); toAdd.push({group:'nodes', data:{id:tgtId, uri:t.object, label:t.object.split('#').pop().split('/').pop()}}); }
      toAdd.push({group:'edges', data:{id:'e'+Math.random(), source:cyId, target:tgtId, label:t.predicate}});
    });
    details.incoming.forEach(t => {
      let srcId;
      let existing = cy.nodes().filter(n => n.data('uri') === t.subject);
      if(existing.length > 0){ srcId = existing[0].id(); }
      else { srcId = 'n' + (++counter); toAdd.push({group:'nodes', data:{id:srcId, uri:t.subject, label:t.subject.split('#').pop().split('/').pop()}}); }
      toAdd.push({group:'edges', data:{id:'e'+Math.random(), source:srcId, target:cyId, label:t.predicate}});
    });
    cy.add(toAdd);
    // Apply colors to new nodes
    cy.nodes().forEach(n => {
      let t = n.data('type');
      if(t && typeColors[t]) n.addClass('type-' + t.replace(/[^a-zA-Z0-9]/g, '_'));
    });
    if(document.getElementById('labelMode').value==='all') applyZoomLabels();
    if(document.getElementById('edgeLabel').checked) updateEdgeLabels();
    cy.layout({ name: document.getElementById('layout').value, animate: true }).run();
    document.getElementById('stats').textContent =
      cy.nodes().length + ' nodes, ' + cy.edges().length + ' edges';
  });
}
function ctxCollapse(){
  hideCtxMenu();
  let targets = cy.nodes(':selected');
  if(targets.length === 0 && ctxNode) targets = ctxNode.collection();
  targets.forEach(n => {
    let hood = n.neighborhood().nodes().filter(nn => nn.degree() <= 1 && !nn.selected());
    hood.connectedEdges().remove();
    hood.remove();
  });
  document.getElementById('stats').textContent =
    cy.nodes().length + ' nodes, ' + cy.edges().length + ' edges';
}
function ctxHide(){
  hideCtxMenu();
  let targets = cy.nodes(':selected');
  if(targets.length === 0 && ctxNode) targets = ctxNode.collection();
  targets.connectedEdges().remove();
  targets.remove();
  document.getElementById('stats').textContent =
    cy.nodes().length + ' nodes, ' + cy.edges().length + ' edges';
}
function ctxPin(){
  hideCtxMenu();
  let targets = cy.nodes(':selected');
  if(targets.length === 0 && ctxNode) targets = ctxNode.collection();
  targets.forEach(n => { if(n.locked()) n.unlock(); else n.lock(); });
}
function updateLabelMode(){
  let mode = document.getElementById('labelMode').value;
  if(mode==='all') applyZoomLabels();
  else cy.nodes().style('label', '');
}
function applyZoomLabels(){
  let zoom = cy.zoom();
  if(zoom > 0.8) cy.nodes().forEach(n => n.style('label', n.data('label')));
  else if(zoom > 0.4) {
    // Only show labels for nodes with few connections or highlighted
    cy.nodes().forEach(n => {
      if(n.degree() <= 3 || n.hasClass('highlighted')) n.style('label', n.data('label'));
      else n.style('label', '');
    });
  }
  else cy.nodes().style('label', '');
}
function updateEdgeLabels(){
  let show = document.getElementById('edgeLabel').checked;
  if(show) cy.edges().forEach(e => {
    let lbl = e.data('label');
    e.style('label', lbl.split('#').pop().split('/').pop());
  });
  else cy.edges().style('label', '');
}
function toggleQueryPanel(){
  let p = document.getElementById('query-panel');
  p.classList.toggle('open');
  if(p.classList.contains('open')){
    document.getElementById('cy').style.height = 'calc(100vh - 50px - 36px - 40vh)';
    document.getElementById('query-input').focus();
  } else {
    document.getElementById('cy').style.height = 'calc(100vh - 50px - 36px)';
  }
  if(cy) cy.resize();
}
function runQuery(){
  let q = document.getElementById('query-input').value.trim();
  if(!q) return;
  document.getElementById('query-status').textContent = 'Running...';
  fetch('/sparql?query=' + encodeURIComponent(q)).then(r=>r.json()).then(data=>{
    if(data.error){
      document.getElementById('query-status').textContent = 'Error: ' + data.error;
      document.getElementById('query-results').innerHTML = '';
      return;
    }
    if(data.boolean !== undefined){
      document.getElementById('query-status').textContent = 'Result: ' + data.boolean;
      document.getElementById('query-results').innerHTML = '';
      return;
    }
    let results = data.results || [];
    document.getElementById('query-status').textContent = results.length + ' results';
    if(results.length === 0){ document.getElementById('query-results').innerHTML = ''; return; }
    let cols = results[0].length;
    let html = '<table><tr>';
    for(let i=0; i<cols; i++) html += '<th>?' + (i+1) + '</th>';
    html += '</tr>';
    results.forEach(row => {
      html += '<tr>';
      row.forEach(cell => {
        let short = String(cell).split('#').pop().split('/').pop();
        let safe = String(cell).replace(/&/g,'&amp;').replace(/</g,'&lt;');
        html += '<td data-uri=\"' + safe + '\" onclick=\"highlightInGraph(this.dataset.uri)\">' + short + '</td>';
      });
      html += '</tr>';
    });
    html += '</table>';
    document.getElementById('query-results').innerHTML = html;
    // Highlight matching nodes
    highlightQueryResults(results);
  }).catch(e => {
    document.getElementById('query-status').textContent = 'Error: ' + e.message;
  });
}
function highlightQueryResults(results){
  cy.elements().removeClass('highlighted dimmed');
  let uris = new Set();
  results.forEach(row => row.forEach(cell => uris.add(String(cell))));
  let matches = cy.nodes().filter(n => uris.has(n.data('uri')));
  if(matches.length > 0){
    matches.addClass('highlighted');
    cy.elements().not(matches).not(matches.connectedEdges()).addClass('dimmed');
  }
}
function highlightInGraph(uri){
  cy.elements().removeClass('highlighted dimmed');
  let matches = cy.nodes().filter(n => n.data('uri') === uri);
  if(matches.length === 0){
    // Node not in view — load it with its neighborhood
    let url = '/api/graph?center=' + encodeURIComponent(uri) + '&depth=1';
    fetch(url).then(r=>r.json()).then(resp=>{
      let data = resp.elements || resp;
      cy.elements().remove();
      cy.add(data);
      cy.nodes().forEach(n => {
        let t = n.data('type');
        if(t && typeColors[t]) n.addClass('type-' + t.replace(/[^a-zA-Z0-9]/g, '_'));
      });
      if(document.getElementById('labelMode').value==='all') applyZoomLabels();
      if(document.getElementById('edgeLabel').checked) updateEdgeLabels();
      cy.layout(getLayoutOpts(false)).run();
      let m = cy.nodes().filter(n => n.data('uri') === uri);
      if(m.length > 0){
        m.addClass('highlighted');
        cy.elements().not(m).not(m.connectedEdges()).addClass('dimmed');
        cy.fit(m, 50);
      }
      document.getElementById('stats').textContent =
        cy.nodes().length + ' nodes, ' + cy.edges().length + ' edges';
      showNodeDetails(uri);
    });
    return;
  }
  matches.addClass('highlighted');
  cy.elements().not(matches).not(matches.connectedEdges()).addClass('dimmed');
  cy.fit(matches, 50);
  showNodeDetails(uri);
}
function showNodeDetails(uri){
  fetch('/api/node?id=' + encodeURIComponent(uri)).then(r=>r.json()).then(details=>{
    let info = '<b>' + details.label + '</b><br>';
    if(details.outgoing && details.outgoing.length > 0){
      info += '<br><u>Properties</u><br>';
      details.outgoing.forEach(t => {
        let pred = t.predicate.split('#').pop().split('/').pop();
        let obj = t.object.split('#').pop().split('/').pop();
        info += '<i>' + pred + '</i>: ' + obj + '<br>';
      });
    }
    if(details.incoming && details.incoming.length > 0){
      info += '<br><u>Referenced by</u><br>';
      details.incoming.forEach(t => {
        let pred = t.predicate.split('#').pop().split('/').pop();
        let subj = t.subject.split('#').pop().split('/').pop();
        info += subj + ' <i>' + pred + '</i><br>';
      });
    }
    let el = document.getElementById('info');
    el.innerHTML = info; el.style.display = 'block';
  });
}
function updateConstruct(){
  let preds = getSelectedPredicates();
  let total = document.querySelectorAll('#pred-panel input').length;
  let where = '';
  if(preds.length > 0 && preds.length < total){
    let alts = preds.map(p => '<' + p + '>').join('|');
    where = '?s ?p ?o . FILTER(?p IN(' + preds.map(p => '<' + p + '>').join(', ') + '))';
  } else {
    where = '?s ?p ?o';
  }
  let q = 'CONSTRUCT { ?s ?p ?o } WHERE { ' + where + ' }';
  document.getElementById('construct-query').textContent = q;
}
function copyConstruct(){
  let q = document.getElementById('construct-query').textContent;
  navigator.clipboard.writeText(q);
}
function runConstruct(){
  let q = document.getElementById('construct-query').textContent;
  let p = document.getElementById('query-panel');
  if(!p.classList.contains('open')) toggleQueryPanel();
  document.getElementById('query-input').value = q;
  runQuery();
}
function exportPNG(){
  let url = cy.png({full:true, scale:2, bg:'#1a1a2e'});
  let a = document.createElement('a');
  a.href = url; a.download = 'graph.png'; a.click();
}
function exportSVG(){
  let url = cy.svg({full:true, scale:1, bg:'#1a1a2e'});
  let blob = new Blob([url], {type:'image/svg+xml'});
  let a = document.createElement('a');
  a.href = URL.createObjectURL(blob); a.download = 'graph.svg'; a.click();
}
</script>
</body></html>"))

;;; ==========================================================================
;;; Server
;;; ==========================================================================

(defun sparql-query-json (g query-string)
  "Execute SPARQL query and return JSON result string."
  (handler-case
      (let ((results (sparql-via-algebra g query-string)))
        (cond
          ((eq results t) (jzon:stringify (let ((ht (make-hash-table :test 'equal)))
                                           (setf (gethash "boolean" ht) t) ht)))
          ((null results) (jzon:stringify (let ((ht (make-hash-table :test 'equal)))
                                           (setf (gethash "boolean" ht) nil) ht)))
          ((listp results)
           (let ((ht (make-hash-table :test 'equal)))
             (setf (gethash "results" ht)
                   (coerce (mapcar (lambda (row)
                                     (coerce (mapcar #'princ-to-string
                                                     (if (listp row) row (list row))) 'vector))
                                   results) 'vector))
             (jzon:stringify ht)))
          (t (jzon:stringify results))))
    (error (e)
      (let ((ht (make-hash-table :test 'equal)))
        (setf (gethash "error" ht) (princ-to-string e))
        (jzon:stringify ht)))))

(defun sparql-update-json (g update-string)
  "Execute SPARQL UPDATE and return JSON result."
  (handler-case
      (let ((count (sparql-update g update-string))
            (ht (make-hash-table :test 'equal)))
        (setf (gethash "success" ht) t)
        (setf (gethash "mutationCount" ht) count)
        (jzon:stringify ht))
    (error (e)
      (let ((ht (make-hash-table :test 'equal)))
        (setf (gethash "error" ht) (princ-to-string e))
        (jzon:stringify ht)))))

(defun start-web-server (graph &key (port 8080))
  "Start the web visualization server for GRAPH on PORT."
  (when *web-server* (stop-web-server))
  (setf *web-graph* graph)
  ;; Define handlers
  (ht:define-easy-handler (handle-index :uri "/") ()
    (setf (ht:content-type*) "text/html")
    (graph-page-html))
  (ht:define-easy-handler (handle-graph-api :uri "/api/graph")
      ((predicates :parameter-type 'string)
       (center :parameter-type 'string)
       (depth :parameter-type 'string)
       (types :parameter-type 'string))
    (setf (ht:content-type*) "application/json")
    (graph-to-cytoscape-json *web-graph*
                             :predicates (when predicates
                                           (cl-ppcre:split "," predicates))
                             :center center
                             :depth (when depth (parse-integer depth :junk-allowed t))
                             :node-types (when types
                                           (cl-ppcre:split "," types))))
  (ht:define-easy-handler (handle-graph-info :uri "/api/info") ()
    (setf (ht:content-type*) "application/json")
    (let ((ht (make-hash-table :test 'equal)))
      (setf (gethash "name" ht) (or (graph-name *web-graph*) "unnamed"))
      (setf (gethash "triples" ht) (triple-count *web-graph*))
      (setf (gethash "subjects" ht) (length (all-subjects *web-graph*)))
      (setf (gethash "predicates" ht) (length (all-predicates *web-graph*)))
      (jzon:stringify ht)))
  (ht:define-easy-handler (handle-predicates-api :uri "/api/predicates") ()
    (setf (ht:content-type*) "application/json")
    (let* ((preds (all-predicates *web-graph*))
           (sorted (sort (mapcar (lambda (p)
                                   (cons p (length (get-triples *web-graph* :predicate p))))
                                 preds)
                         #'> :key #'cdr)))
      (jzon:stringify (coerce (mapcar #'car sorted) 'vector))))
  (ht:define-easy-handler (handle-sparql :uri "/sparql")
      ((query :parameter-type 'string))
    (setf (ht:content-type*) "application/json")
    (sparql-query-json *web-graph* query))
  (ht:define-easy-handler (handle-sparql-update :uri "/update")
      ((update :parameter-type 'string))
    (setf (ht:content-type*) "application/json")
    (sparql-update-json *web-graph* update))
  (ht:define-easy-handler (handle-types-api :uri "/api/types") ()
    (setf (ht:content-type*) "application/json")
    (graph-types-json *web-graph*))
  (ht:define-easy-handler (handle-type-counts-api :uri "/api/type-counts") ()
    (setf (ht:content-type*) "application/json")
    (let ((counts (make-hash-table :test 'equal)))
      (dolist (tr (append (get-triples *web-graph* :predicate "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                          (get-triples *web-graph* :predicate "rdf:type")))
        (incf (gethash (princ-to-string (triple-object tr)) counts 0)))
      (jzon:stringify counts)))
  (ht:define-easy-handler (handle-node-api :uri "/api/node")
      ((id :parameter-type 'string))
    (setf (ht:content-type*) "application/json")
    (node-info-json *web-graph* id))
  (setf *web-server*
        (make-instance 'ht:easy-acceptor :port port))
  (ht:start *web-server*)
  (format t "Ariadne web explorer at http://localhost:~A/~%" port)
  *web-server*)

(defun stop-web-server ()
  "Stop the web visualization server."
  (when *web-server*
    (ht:stop *web-server*)
    (setf *web-server* nil *web-graph* nil)))
