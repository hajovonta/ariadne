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
