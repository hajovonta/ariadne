;;;; web-server.lisp
;;;; Interactive graph visualization with Hunchentoot + Cytoscape.js

(in-package #:ariadne)

(defvar *web-server* nil)
(defvar *web-graph* nil)
(defvar *explorer-pushed* nil "Pushed query/results for the explorer to pick up.")

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
    ;; Collect nodes and edges (skip RDF literals)
    (dolist (tr triples)
      (let* ((subj (princ-to-string (triple-subject tr)))
             (obj-raw (triple-object tr))
             (obj (princ-to-string obj-raw)))
        (setf (gethash subj nodes) t)
        (unless (typep obj-raw 'rdf-literal)
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
  (let ((path (merge-pathnames "static/index.html" (asdf:system-source-directory :ariadne))))
    (uiop:read-file-string path)))

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
  (ht:define-easy-handler (handle-pushed-api :uri "/api/pushed") ()
    (setf (ht:content-type*) "application/json")
    (if *explorer-pushed*
        (prog1 (jzon:stringify *explorer-pushed*)
          (setf *explorer-pushed* nil))
        "null"))
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

(defun explorer-query (query)
  "Push a query to the Graph Explorer. QUERY can be:
   - A string: executed as SPARQL
   - A list: executed as CL DSL query
   Results are displayed in the explorer on next poll."
  (let* ((results (if (stringp query)
                      (sparql-via-algebra *web-graph* query)
                      (query *web-graph* query)))
         (ht (make-hash-table :test 'equal)))
    (setf (gethash "query" ht) (if (stringp query) query (princ-to-string query)))
    (setf (gethash "results" ht)
          (cond
            ((eq results t) "true")
            ((null results) "false")
            ((listp results)
             (coerce (mapcar (lambda (row)
                               (coerce (mapcar #'princ-to-string
                                               (if (listp row) row (list row))) 'vector))
                             results) 'vector))
            (t results)))
    (setf *explorer-pushed* ht)
    (length (if (vectorp (gethash "results" ht))
                (gethash "results" ht)
                #()))))

(defun explorer-focus (node-uri &key (depth 2))
  "Push a focus command to the Graph Explorer — recenters on NODE-URI with DEPTH hops."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "focus" ht) node-uri)
    (setf (gethash "depth" ht) depth)
    (setf *explorer-pushed* ht)
    node-uri))
