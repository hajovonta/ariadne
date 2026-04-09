;;;; analytics.lisp
;;;; Graph analytics: degree centrality, connected components, PageRank, clustering

(in-package #:ariadne)

;;; ==========================================================================
;;; Degree Centrality
;;; ==========================================================================

(defun degree-centrality (g predicate &key (direction :out))
  "Compute degree centrality for all nodes connected by PREDICATE.
Returns an alist of (node . degree)."
  (let ((degrees (make-hash-table :test 'equal)))
    (dolist (tr (get-triples g :predicate predicate))
      (ecase direction
        (:out (incf (gethash (triple-subject tr) degrees 0)))
        (:in (incf (gethash (triple-object tr) degrees 0)))
        (:both (incf (gethash (triple-subject tr) degrees 0))
               (incf (gethash (triple-object tr) degrees 0)))))
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) degrees)
      result)))

;;; ==========================================================================
;;; Connected Components (undirected)
;;; ==========================================================================

(defun connected-components (g predicate)
  "Find connected components treating PREDICATE edges as undirected.
Returns a list of lists, each being a component's node set."
  ;; Build adjacency
  (let ((adj (make-hash-table :test 'equal))
        (visited (make-hash-table :test 'equal)))
    (dolist (tr (get-triples g :predicate predicate))
      (let ((s (triple-subject tr))
            (o (triple-object tr)))
        (push o (gethash s adj))
        (push s (gethash o adj))))
    ;; BFS from each unvisited node
    (let (components)
      (maphash (lambda (node _)
                 (declare (ignore _))
                 (unless (gethash node visited)
                   (let ((component nil)
                         (queue (list node)))
                     (setf (gethash node visited) t)
                     (loop while queue do
                       (let ((current (pop queue)))
                         (push current component)
                         (dolist (neighbor (gethash current adj))
                           (unless (gethash neighbor visited)
                             (setf (gethash neighbor visited) t)
                             (push neighbor queue)))))
                     (push component components))))
               adj)
      components)))

;;; ==========================================================================
;;; PageRank
;;; ==========================================================================

(defun pagerank (g predicate &key (damping 0.85) (iterations 20))
  "Compute PageRank for all nodes connected by PREDICATE.
Returns an alist of (node . rank)."
  (let ((nodes (make-hash-table :test 'equal))
        (out-edges (make-hash-table :test 'equal))
        (in-edges (make-hash-table :test 'equal)))
    ;; Build graph structure
    (dolist (tr (get-triples g :predicate predicate))
      (let ((s (triple-subject tr))
            (o (triple-object tr)))
        (setf (gethash s nodes) t)
        (setf (gethash o nodes) t)
        (push o (gethash s out-edges))
        (push s (gethash o in-edges))))
    (let* ((n (hash-table-count nodes))
           (ranks (make-hash-table :test 'equal))
           (init (if (> n 0) (/ 1.0 n) 0.0)))
      ;; Initialize
      (maphash (lambda (node _) (declare (ignore _))
                 (setf (gethash node ranks) init))
               nodes)
      ;; Iterate
      (dotimes (i iterations)
        (let ((new-ranks (make-hash-table :test 'equal)))
          (maphash (lambda (node _) (declare (ignore _))
                     (let ((sum 0.0))
                       (dolist (src (gethash node in-edges))
                         (let ((out-deg (length (gethash src out-edges))))
                           (when (> out-deg 0)
                             (incf sum (/ (gethash src ranks 0.0) out-deg)))))
                       (setf (gethash node new-ranks)
                             (+ (/ (- 1.0 damping) n)
                                (* damping sum)))))
                   nodes)
          (setf ranks new-ranks)))
      ;; Return as alist
      (let (result)
        (maphash (lambda (k v) (push (cons k v) result)) ranks)
        result))))

;;; ==========================================================================
;;; Clustering Coefficient
;;; ==========================================================================

(defun clustering-coefficient (g node predicate)
  "Compute the local clustering coefficient for NODE.
Measures how connected a node's neighbors are to each other.
Returns a value between 0.0 and 1.0."
  (let ((neighbors nil))
    ;; Collect all neighbors (outgoing)
    (dolist (tr (get-triples g :subject node :predicate predicate))
      (pushnew (triple-object tr) neighbors :test #'equal))
    (let ((k (length neighbors)))
      (if (< k 2)
          0.0
          (let ((edges-between 0))
            ;; Count edges between neighbors
            (dolist (n1 neighbors)
              (dolist (n2 neighbors)
                (unless (equal n1 n2)
                  (when (has-triple-p g n1 predicate n2)
                    (incf edges-between)))))
            ;; Clustering coefficient = actual / possible edges
            (/ (float edges-between) (* k (1- k))))))))
