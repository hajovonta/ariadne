;;;; web-server.lisp
;;;; Interactive graph visualization with Hunchentoot + Cytoscape.js

(in-package #:ariadne)

(defvar *web-server* nil)
(defvar *web-graph* nil)

;;; ==========================================================================
;;; JSON conversion
;;; ==========================================================================

(defun json-escape (str)
  (with-output-to-string (s)
    (loop for c across (princ-to-string str) do
      (case c
        (#\" (write-string "\\\"" s))
        (#\\ (write-string "\\\\" s))
        (#\Newline (write-string "\\n" s))
        (#\Tab (write-string "\\t" s))
        (t (write-char c s))))))

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
                                      (when (and (> d 0) (not (gethash node reachable)))
                                        (setf (gethash node reachable) t)
                                        (dolist (tr trs)
                                          (when (equal (triple-subject tr) node)
                                            (walk (triple-object tr) (1- d)))
                                          (when (equal (triple-object tr) node)
                                            (walk (triple-subject tr) (1- d)))))))
                             (setf (gethash center reachable) t)
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
    (with-output-to-string (s)
      (write-string "[" s)
      (let ((first t))
        ;; Nodes
        (maphash (lambda (id v)
                   (declare (ignore v))
                   (if first (setf first nil) (write-string "," s))
                   (format s "{\"data\":{\"id\":\"~A\",\"label\":\"~A\"}}"
                           (json-escape id)
                           (json-escape (node-label id))))
                 nodes)
        ;; Edges
        (let ((eid 0))
          (dolist (tr edges)
            (write-string "," s)
            (format s "{\"data\":{\"id\":\"e~A\",\"source\":\"~A\",\"target\":\"~A\",\"label\":\"~A\"}}"
                    (incf eid)
                    (json-escape (princ-to-string (triple-subject tr)))
                    (json-escape (princ-to-string (triple-object tr)))
                    (json-escape (triple-predicate tr))))))
      (write-string "]" s))))

(defun node-label (id)
  "Short label for a node: strip URI prefix."
  (let ((s (princ-to-string id)))
    (or (let ((hash (position #\# s :from-end t)))
          (when hash (subseq s (1+ hash))))
        (let ((slash (position #\/ s :from-end t)))
          (when (and slash (> slash 8)) (subseq s (1+ slash))))
        s)))

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
  <button onclick='cy.fit()'>Fit</button>
  <button onclick='selectAll()'>All predicates</button>
  <span id='stats'></span>
</div>
<div id='cy'></div>
<div id='pred-panel'></div>
<div id='info'></div>
<script>
let cy;
// Load predicate list
fetch('/api/predicates').then(r=>r.json()).then(preds=>{
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
    cy = cytoscape({
      container: document.getElementById('cy'),
      elements: data,
      style: [
        { selector: 'node', style: {
          'label': '', 'background-color': '#e94560',
          'width': 14, 'height': 14 }},
        { selector: 'node:active, node:selected', style: {
          'label': 'data(label)', 'color': '#eee', 'font-size': '11px',
          'text-valign': 'bottom', 'text-margin-y': 4 }},
        { selector: 'edge', style: {
          'curve-style': 'bezier',
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
    document.getElementById('stats').textContent =
      cy.nodes().length + ' nodes, ' + cy.edges().length + ' edges';
    cy.on('mouseover', 'node', function(e){
      e.target.style('label', e.target.data('label'));
      e.target.style('color', '#eee');
      e.target.style('font-size', '11px');
      e.target.style('text-valign', 'bottom');
      e.target.style('text-margin-y', 4);
    });
    cy.on('mouseout', 'node', function(e){
      if(!e.target.hasClass('highlighted'))
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
</script>
</body></html>"))

;;; ==========================================================================
;;; Server
;;; ==========================================================================

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
    (format nil "{\"name\":\"~A\",\"triples\":~A,\"subjects\":~A,\"predicates\":~A}"
            (json-escape (or (graph-name *web-graph*) "unnamed"))
            (triple-count *web-graph*)
            (length (all-subjects *web-graph*))
            (length (all-predicates *web-graph*))))
  (ht:define-easy-handler (handle-predicates-api :uri "/api/predicates") ()
    (setf (ht:content-type*) "application/json")
    ;; Return predicates sorted by frequency (most common first)
    (let* ((preds (all-predicates *web-graph*))
           (sorted (sort (mapcar (lambda (p)
                                   (cons p (length (get-triples *web-graph* :predicate p))))
                                 preds)
                         #'> :key #'cdr)))
      (format nil "[~{\"~A\"~^,~}]"
              (mapcar (lambda (pc) (json-escape (car pc))) sorted))))
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
