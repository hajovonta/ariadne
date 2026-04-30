;;;; web-server.lisp
;;;; Interactive graph visualization with Hunchentoot + Cytoscape.js

(in-package #:ariadne)

(defvar *web-server* nil)
(defvar *web-graph* nil)

;;; ==========================================================================
;;; JSON conversion
;;; ==========================================================================

(defun graph-to-cytoscape-json (g &key predicates center depth)
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
                                      (when (and (>= d 0) (not (gethash node reachable)))
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
    ;; Collect nodes and edges
    (dolist (tr triples)
      (setf (gethash (princ-to-string (triple-subject tr)) nodes) t)
      (setf (gethash (princ-to-string (triple-object tr)) nodes) t)
      (push tr edges))
    ;; Build JSON
    (let ((elements nil))
      ;; Build node type lookup
      (let ((node-types (make-hash-table :test 'equal)))
        (dolist (tr (get-triples g :predicate "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
          (setf (gethash (princ-to-string (triple-subject tr)) node-types)
                (princ-to-string (triple-object tr))))
        ;; Nodes
        (maphash (lambda (id v)
                   (declare (ignore v))
                   (let ((data (make-hash-table :test 'equal)))
                     (setf (gethash "id" data) id)
                     (setf (gethash "label" data) (node-label id))
                     (let ((typ (gethash id node-types)))
                       (when typ (setf (gethash "type" data) typ)))
                     (let ((el (make-hash-table :test 'equal)))
                       (setf (gethash "data" el) data)
                       (push el elements))))
                 nodes))
      ;; Edges
      (let ((eid 0))
        (dolist (tr edges)
          (let ((data (make-hash-table :test 'equal)))
            (setf (gethash "id" data) (format nil "e~A" (incf eid)))
            (setf (gethash "source" data) (princ-to-string (triple-subject tr)))
            (setf (gethash "target" data) (princ-to-string (triple-object tr)))
            (setf (gethash "label" data) (triple-predicate tr))
            (let ((el (make-hash-table :test 'equal)))
              (setf (gethash "data" el) data)
              (push el elements)))))
      (jzon:stringify (coerce (nreverse elements) 'vector)))))

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
                       (get-triples g :predicate "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")))
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
<style>
  body { margin: 0; font-family: sans-serif; background: #1a1a2e; color: #eee; }
  #cy { width: 100%%; height: calc(100vh - 50px); }
  #toolbar { height: 50px; display: flex; align-items: center; padding: 0 16px; gap: 12px; background: #16213e; flex-wrap: wrap; }
  #toolbar input, #toolbar select, #toolbar button {
    padding: 6px 10px; border-radius: 4px; border: 1px solid #444; background: #0f3460; color: #eee; }
  #toolbar button { cursor: pointer; }
  #toolbar button:hover { background: #e94560; }
  #info { position: fixed; bottom: 16px; right: 16px; background: #16213e; padding: 12px;
    border-radius: 8px; max-width: 350px; font-size: 13px; display: none; border: 1px solid #333; }
  #predicates { max-width: 300px; }
  #pred-panel { position: fixed; top: 50px; left: 0; background: #16213e; padding: 12px;
    border-right: 1px solid #333; border-bottom: 1px solid #333; border-radius: 0 0 8px 0;
    max-height: 80vh; overflow-y: auto; display: none; min-width: 200px; z-index: 10; }
  #pred-panel label { display: block; padding: 3px 0; cursor: pointer; font-size: 13px; }
  #pred-panel label:hover { color: #e94560; }
  #pred-panel input { margin-right: 6px; }
  #legend { position: fixed; top: 50px; right: 0; background: #16213e; padding: 12px;
    border-left: 1px solid #333; border-bottom: 1px solid #333; border-radius: 0 0 0 8px;
    font-size: 12px; display: none; z-index: 10; }
  .legend-item { padding: 2px 0; display: flex; align-items: center; gap: 6px; }
  .legend-swatch { width: 12px; height: 12px; border-radius: 50%%; display: inline-block; }
</style>
</head><body>
<div id='toolbar'>
  <strong>Ariadne</strong>
  <input id='search' placeholder='Search nodes...' oninput='searchNodes()'>
  <select id='predicates' multiple title='Filter predicates (ctrl+click)' style='display:none'></select>
  <button onclick='togglePredPanel()'>Predicates ▼</button>
  <button onclick='loadGraph()'>Apply</button>
  <select id='layout' onchange='changeLayout()'>
    <option value='cose'>Force-directed</option>
    <option value='breadthfirst'>Hierarchical</option>
    <option value='circle'>Circular</option>
    <option value='grid'>Grid</option>
    <option value='concentric'>Concentric</option>
  </select>
  <select id='labelMode' onchange='updateLabelMode()'>
    <option value='hover'>Labels: hover</option>
    <option value='all'>Labels: all</option>
    <option value='none'>Labels: none</option>
  </select>
  <label><input type='checkbox' id='edgeLabel' onchange='updateEdgeLabels()'> Edge labels</label>
  <button onclick='cy.fit()'>Fit</button>
  <button onclick='selectAll()'>All predicates</button>
  <span id='stats'></span>
</div>
<div id='cy'></div>
<div id='pred-panel'></div>
<div id='legend'></div>
<div id='info'></div>
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
}
function loadGraph(){
  let selected = getSelectedPredicates();
  let total = document.querySelectorAll('#pred-panel input').length;
  let url = '/api/graph';
  if(selected.length > 0 && selected.length < total)
    url += '?predicates=' + encodeURIComponent(selected.join(','));
  fetch(url).then(r=>r.json()).then(data=>{
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
          'color': '#eee', 'font-size': '11px',
          'text-valign': 'bottom', 'text-margin-y': 4,
          'width': 14, 'height': 14 }},
        { selector: 'edge', style: {
          'curve-style': 'bezier',
          'label': showEdgeLabels ? 'data(label)' : '',
          'font-size': '9px', 'color': '#888', 'text-rotation': 'autorotate',
          'target-arrow-shape': 'triangle', 'line-color': '#0f3460',
          'target-arrow-color': '#0f3460', 'width': 1.5, 'opacity': 0.6 }},
        { selector: ':selected', style: { 'background-color': '#ffd700', 'line-color': '#ffd700' }},
        { selector: '.highlighted', style: {
          'background-color': '#ffd700', 'label': 'data(label)',
          'color': '#eee', 'font-size': '11px', 'text-valign': 'bottom', 'text-margin-y': 4 }},
        { selector: '.dimmed', style: { opacity: 0.08 }}
      ],
      layout: { name: document.getElementById('layout').value, animate: false },
      wheelSensitivity: 0.15,
      minZoom: 0.1,
      maxZoom: 10
    });
    // Apply type colors
    cy.nodes().forEach(n => {
      let t = n.data('type');
      if(t && typeColors[t]) n.style('background-color', typeColors[t]);
    });
    document.getElementById('stats').textContent =
      cy.nodes().length + ' nodes, ' + cy.edges().length + ' edges';
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
      let info = '<b>' + n.data('id').split('#').pop().split('/').pop() + '</b><br>';
      n.connectedEdges().forEach(e => {
        let other = e.source().id() === n.id() ? e.target() : e.source();
        info += '<i>' + e.data('label').split('#').pop().split('/').pop() + '</i> → '
          + other.data('label') + '<br>';
      });
      let el = document.getElementById('info');
      el.innerHTML = info; el.style.display = 'block';
    });
    cy.on('tap', function(e){ if(e.target===cy){
      cy.elements().removeClass('highlighted dimmed');
      document.getElementById('info').style.display='none';
    }});
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
function changeLayout(){
  cy.layout({ name: document.getElementById('layout').value, animate: true }).run();
}
function selectAll(){
  document.querySelectorAll('#pred-panel input').forEach(cb => cb.checked = true);
  loadGraph();
}
function updateLabelMode(){
  let mode = document.getElementById('labelMode').value;
  if(mode==='all') cy.nodes().style('label','data(label)');
  else if(mode==='none') cy.nodes().style('label','');
  else cy.nodes().style('label','');
}
function updateEdgeLabels(){
  let show = document.getElementById('edgeLabel').checked;
  cy.edges().style('label', show ? 'data(label)' : '');
  cy.edges().style('font-size', '9px');
  cy.edges().style('color', '#888');
  cy.edges().style('text-rotation', 'autorotate');
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
                                     (coerce (if (listp row) row (list row)) 'vector))
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
       (depth :parameter-type 'string))
    (setf (ht:content-type*) "application/json")
    (graph-to-cytoscape-json *web-graph*
                             :predicates (when predicates
                                           (cl-ppcre:split "," predicates))
                             :center center
                             :depth (when depth (parse-integer depth :junk-allowed t))))
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
