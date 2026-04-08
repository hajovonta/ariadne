;;;; property-graph.lisp
;;;; Property graph model layered on the triple store

(in-package #:ariadne)

;;; Property graph is implemented on top of triples using conventions:
;;;   (node-id :ariadne/type :ariadne/node)     - marks a node
;;;   (node-id :ariadne/label label)             - node label
;;;   (node-id :ariadne/prop/KEY value)          - node property
;;;   (edge-id :ariadne/type :ariadne/edge)      - marks an edge
;;;   (edge-id :ariadne/from node-id)            - edge source
;;;   (edge-id :ariadne/to node-id)              - edge target
;;;   (edge-id :ariadne/edge-type type)          - edge type
;;;   (edge-id :ariadne/eprop/KEY value)         - edge property

(defvar *edge-counter* 0)

(defun prop-key (name) (intern (format nil "ARIADNE/PROP/~A" name) :keyword))
(defun eprop-key (name) (intern (format nil "ARIADNE/EPROP/~A" name) :keyword))

;;; ==========================================================================
;;; Nodes
;;; ==========================================================================

(defstruct (pg-node (:constructor %make-pg-node (id))) id)

(defun add-node (g id &key properties labels)
  (add-triple g id :ariadne/type :ariadne/node)
  (dolist (prop properties)
    (add-triple g id (prop-key (car prop)) (cdr prop)))
  (dolist (label labels)
    (add-triple g id :ariadne/label label))
  (%make-pg-node id))

(defun node-id (n) (pg-node-id n))

(defun get-node (g id)
  (when (has-triple-p g id :ariadne/type :ariadne/node)
    (%make-pg-node id)))

(defun node-property (g id prop)
  (let ((triples (get-triples g :subject id :predicate (prop-key prop))))
    (when triples (triple-object (first triples)))))

(defun set-node-property (g id prop value)
  (let ((pk (prop-key prop)))
    (remove-triples g :subject id :predicate pk)
    (add-triple g id pk value)))

(defun remove-node-property (g id prop)
  (remove-triples g :subject id :predicate (prop-key prop)))

(defun node-properties (g id)
  (let ((prefix "ARIADNE/PROP/")
        (results nil))
    (dolist (tr (get-triples g :subject id) results)
      (let ((pname (symbol-name (triple-predicate tr))))
        (when (and (> (length pname) (length prefix))
                   (string= prefix pname :end2 (length prefix)))
          (push (cons (intern (subseq pname (length prefix)) :keyword)
                      (triple-object tr))
                results))))))

(defun node-labels (g id)
  (mapcar #'triple-object (get-triples g :subject id :predicate :ariadne/label)))

(defun find-nodes (g &key label)
  (when label
    (mapcar (lambda (tr) (%make-pg-node (triple-subject tr)))
            (get-triples g :predicate :ariadne/label :object label))))

(defun remove-node (g id)
  ;; Remove all edges from/to this node
  (dolist (tr (get-triples g :predicate :ariadne/from :object id))
    (remove-edge-by-id g (triple-subject tr)))
  (dolist (tr (get-triples g :predicate :ariadne/to :object id))
    (remove-edge-by-id g (triple-subject tr)))
  ;; Remove all triples with this node as subject
  (remove-triples g :subject id))

;;; ==========================================================================
;;; Edges
;;; ==========================================================================

(defstruct (pg-edge (:constructor %make-pg-edge (id from to edge-type)))
  id from to edge-type)

(defun edge-from (e) (pg-edge-from e))
(defun edge-to (e) (pg-edge-to e))
(defun edge-type (e) (pg-edge-edge-type e))

(defun make-edge-id ()
  (intern (format nil "ARIADNE/EDGE/~A" (incf *edge-counter*)) :keyword))

(defun add-edge (g from to type &key properties)
  (let ((eid (make-edge-id)))
    (add-triple g eid :ariadne/type :ariadne/edge)
    (add-triple g eid :ariadne/from from)
    (add-triple g eid :ariadne/to to)
    (add-triple g eid :ariadne/edge-type type)
    (dolist (prop properties)
      (add-triple g eid (eprop-key (car prop)) (cdr prop)))
    (%make-pg-edge eid from to type)))

(defun edge-property (g edge prop)
  "Get a property from an edge by looking it up in the graph's triple store."
  (let ((triples (get-triples g :subject (pg-edge-id edge) :predicate (eprop-key prop))))
    (when triples (triple-object (first triples)))))

(defun get-edges (g &key from to type)
  "Get edges matching constraints."
  (let ((edge-ids nil))
    (cond
      (from
       (dolist (tr (get-triples g :predicate :ariadne/from :object from))
         (push (triple-subject tr) edge-ids)))
      (to
       (dolist (tr (get-triples g :predicate :ariadne/to :object to))
         (push (triple-subject tr) edge-ids)))
      (t
       (dolist (tr (get-triples g :predicate :ariadne/type :object :ariadne/edge))
         (push (triple-subject tr) edge-ids))))
    ;; Filter by type if specified
    (let ((results nil))
      (dolist (eid edge-ids results)
        (let ((e-from (triple-object (first (get-triples g :subject eid :predicate :ariadne/from))))
              (e-to (triple-object (first (get-triples g :subject eid :predicate :ariadne/to))))
              (e-type (triple-object (first (get-triples g :subject eid :predicate :ariadne/edge-type)))))
          (when (and (or (null from) (equal from e-from))
                     (or (null to) (equal to e-to))
                     (or (null type) (equal type e-type)))
            (push (%make-pg-edge eid e-from e-to e-type) results)))))))

(defun remove-edge (g from to type)
  (dolist (e (get-edges g :from from :to to :type type))
    (remove-edge-by-id g (pg-edge-id e))))

(defun remove-edge-by-id (g eid)
  (remove-triples g :subject eid))

;;; ==========================================================================
;;; Neighbors
;;; ==========================================================================

(defun neighbors (g id &key (direction :out) type)
  (let ((results nil))
    (when (member direction '(:out :both))
      (dolist (e (get-edges g :from id :type type))
        (pushnew (pg-edge-to e) results :test #'equal)))
    (when (member direction '(:in :both))
      (dolist (e (get-edges g :to id :type type))
        (pushnew (pg-edge-from e) results :test #'equal)))
    results))
