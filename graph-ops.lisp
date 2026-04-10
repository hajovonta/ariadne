;;;; graph-ops.lisp
;;;; Graph operations: merge, diff, copy

(in-package #:ariadne)

(defun merge-graphs (g1 g2)
  "Create a new graph containing all triples from G1 and G2."
  (let ((result (make-graph :name (graph-name g1))))
    (dolist (tr (get-triples g1))
      (add-triple result (triple-subject tr) (triple-predicate tr) (triple-object tr)))
    (dolist (tr (get-triples g2))
      (add-triple result (triple-subject tr) (triple-predicate tr) (triple-object tr)))
    result))

(defun merge-graphs-into (target source)
  "Add all triples from SOURCE into TARGET."
  (dolist (tr (get-triples source))
    (add-triple target (triple-subject tr) (triple-predicate tr) (triple-object tr)))
  target)

(defun diff-graphs (g1 g2)
  "Return triples in G1 that are not in G2."
  (remove-if (lambda (tr)
               (has-triple-p g2 (triple-subject tr) (triple-predicate tr) (triple-object tr)))
             (get-triples g1)))

(defun copy-graph (g)
  "Create an independent deep copy of G."
  (let ((result (make-graph :name (graph-name g))))
    (dolist (tr (get-triples g))
      (add-triple result (triple-subject tr) (triple-predicate tr) (triple-object tr)))
    result))


;;; ==========================================================================
;;; Export: Cytoscape JSON
;;; ==========================================================================

(defun export-cytoscape-json (g &key predicates center depth)
  "Export graph as Cytoscape.js compatible JSON elements array."
  (graph-to-cytoscape-json g :predicates predicates :center center :depth depth))

;;; ==========================================================================
;;; Export: JSON-LD
;;; ==========================================================================

(defun export-json-ld (g)
  "Export graph as JSON-LD string."
  (let ((by-subject (make-hash-table :test 'equal))
        (rdf-type "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
    (dolist (tr (get-triples g))
      (push tr (gethash (triple-subject tr) by-subject)))
    (let ((objects nil))
      (maphash
       (lambda (subj triples)
         (let ((ht (make-hash-table :test 'equal)))
           (setf (gethash "@id" ht) subj)
           (dolist (tr triples)
             (let ((pred (triple-predicate tr))
                   (obj (triple-object tr)))
               (if (equal pred rdf-type)
                   (setf (gethash "@type" ht) obj)
                   (setf (gethash pred ht)
                         (if (and (stringp obj) (search "://" obj))
                             (let ((ref (make-hash-table :test 'equal)))
                               (setf (gethash "@id" ref) obj) ref)
                             obj)))))
           (push ht objects)))
       by-subject)
      (jzon:stringify (coerce (nreverse objects) 'vector)))))


;;; ==========================================================================
;;; JSON-LD Import
;;; ==========================================================================

(defun import-json-ld (g data)
  "Import JSON-LD string into graph G."
  (let ((parsed (jzon:parse data))
        (rdf-type "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
    (loop for obj across parsed do
      (let ((id (gethash "@id" obj)))
        (when id
          (maphash (lambda (key val)
                     (cond
                       ((equal key "@id") nil)
                       ((equal key "@type")
                        (add-triple g id rdf-type val))
                       ((and (hash-table-p val) (gethash "@id" val))
                        (add-triple g id key (gethash "@id" val)))
                       (t (add-triple g id key val))))
                   obj))))))

(defun blank-node-p (s)
  "Return T if S is a blank node string."
  (and (stringp s) (>= (length s) 2) (char= (char s 0) #\_) (char= (char s 1) #\:)))

(defun skolemize-blank-nodes (g &key (base "https://ariadne.example"))
  "Replace all blank nodes in G with stable well-known URIs."
  (let ((mapping (make-hash-table :test 'equal))
        (triples (get-triples g)))
    (flet ((skolem-uri (bnode)
             (or (gethash bnode mapping)
                 (setf (gethash bnode mapping)
                       (format nil "~A/.well-known/genid/~A" base (subseq bnode 2))))))
      (dolist (tr triples)
        (let ((s (triple-subject tr))
              (p (triple-predicate tr))
              (o (triple-object tr)))
          (when (or (blank-node-p s) (blank-node-p o))
            (remove-triple g s p o)
            (add-triple g
                        (if (blank-node-p s) (skolem-uri s) s)
                        p
                        (if (blank-node-p o) (skolem-uri o) o)))))))
  g)

;;; ==========================================================================
;;; Graph Versioning
;;; ==========================================================================

(defun graph-versions (g)
  "Return list of version plists (:name :timestamp) for graph G."
  (getf (graph-extra g) :versions))

(defun graph-checkpoint (g name)
  "Save current graph state as a named version."
  (let* ((snap (mapcar (lambda (tr)
                         (list (triple-subject tr)
                               (triple-predicate tr)
                               (triple-object tr)))
                       (get-triples g)))
         (entry (list :name name
                      :timestamp (get-universal-time)
                      :triples snap)))
    (setf (getf (graph-extra g) :versions)
          (append (graph-versions g) (list entry)))
    name))

(defun graph-restore (g name)
  "Restore graph G to the named version."
  (let ((version (find name (graph-versions g)
                       :key (lambda (v) (getf v :name))
                       :test #'string=)))
    (unless version (error "Unknown version: ~A" name))
    (clear-graph g)
    (dolist (spo (getf version :triples))
      (add-triple g (first spo) (second spo) (third spo)))
    g))

(defun query-at-version (g name expr)
  "Execute query EXPR against the named version of G without modifying G."
  (let ((tmp (make-graph :name (format nil "~A@~A" (graph-name g) name)))
        (version (find name (graph-versions g)
                       :key (lambda (v) (getf v :name))
                       :test #'string=)))
    (unless version (error "Unknown version: ~A" name))
    (dolist (spo (getf version :triples))
      (add-triple tmp (first spo) (second spo) (third spo)))
    (query tmp expr)))
