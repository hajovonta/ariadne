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
    (with-output-to-string (s)
      (write-string "[" s)
      (let ((first-subj t))
        (maphash
         (lambda (subj triples)
           (if first-subj (setf first-subj nil) (write-string "," s))
           (format s "{\"@id\":\"~A\"" (json-escape subj))
           ;; Collect types and properties
           (dolist (tr triples)
             (let ((pred (triple-predicate tr))
                   (obj (triple-object tr)))
               (if (equal pred rdf-type)
                   (format s ",\"@type\":\"~A\"" (json-escape obj))
                   (format s ",\"~A\":~A"
                           (json-escape pred)
                           (json-ld-value obj)))))
           (write-string "}" s))
         by-subject))
      (write-string "]" s))))

(defun json-ld-value (obj)
  (cond
    ((stringp obj)
     (if (and (> (length obj) 0) (search "://" obj))
         (format nil "{\"@id\":\"~A\"}" (json-escape obj))
         (format nil "\"~A\"" (json-escape obj))))
    ((numberp obj) (format nil "~A" obj))
    ((eq obj t) "true")
    ((null obj) "false")
    (t (format nil "\"~A\"" (json-escape (princ-to-string obj))))))
