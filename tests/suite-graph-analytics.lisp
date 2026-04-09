;;;; tests/suite-graph-analytics.lisp
;;;; Graph analytics: PageRank, connected components, degree centrality

(in-package #:ariadne/tests)
(in-suite :graph-analytics)

;; =============================================================================
;; Degree Centrality
;; =============================================================================

(test degree-centrality-out
  "Out-degree centrality counts outgoing edges"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (add-triple g "alice" "knows" "dave")
    (add-triple g "bob" "knows" "charlie")
    (let ((degrees (degree-centrality g "knows" :direction :out)))
      (is (= 3 (cdr (assoc "alice" degrees :test #'equal))))
      (is (= 1 (cdr (assoc "bob" degrees :test #'equal)))))))

(test degree-centrality-in
  "In-degree centrality counts incoming edges"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "charlie" "knows" "bob")
    (add-triple g "dave" "knows" "bob")
    (let ((degrees (degree-centrality g "knows" :direction :in)))
      (is (= 3 (cdr (assoc "bob" degrees :test #'equal)))))))

;; =============================================================================
;; Connected Components
;; =============================================================================

(test connected-components-single
  "Single connected component"
  (let ((g (make-graph)))
    (add-triple g "a" "link" "b")
    (add-triple g "b" "link" "c")
    (add-triple g "c" "link" "d")
    (let ((components (connected-components g "link")))
      (is (= 1 (length components)))
      (is (= 4 (length (first components)))))))

(test connected-components-multiple
  "Multiple disconnected components"
  (let ((g (make-graph)))
    (add-triple g "a" "link" "b")
    (add-triple g "c" "link" "d")
    (add-triple g "e" "link" "f")
    (let ((components (connected-components g "link")))
      (is (= 3 (length components)))
      (is (every (lambda (c) (= 2 (length c))) components)))))

(test connected-components-isolated
  "Isolated nodes form their own components"
  (let ((g (make-graph)))
    (add-triple g "a" "link" "b")
    (add-triple g "c" "type" "node")
    ;; c is not connected via "link"
    (let ((components (connected-components g "link")))
      (is (= 1 (length components)))
      (is (= 2 (length (first components)))))))

;; =============================================================================
;; PageRank
;; =============================================================================

(test pagerank-basic
  "PageRank assigns higher rank to well-connected nodes"
  (let ((g (make-graph)))
    ;; Star topology: everyone points to center
    (add-triple g "a" "links" "center")
    (add-triple g "b" "links" "center")
    (add-triple g "c" "links" "center")
    (add-triple g "d" "links" "center")
    (add-triple g "center" "links" "a")
    (let ((ranks (pagerank g "links")))
      ;; Center should have highest rank
      (let ((center-rank (cdr (assoc "center" ranks :test #'equal)))
            (a-rank (cdr (assoc "a" ranks :test #'equal))))
        (is (> center-rank a-rank))))))

(test pagerank-equal-graph
  "PageRank on a cycle gives roughly equal ranks"
  (let ((g (make-graph)))
    (add-triple g "a" "next" "b")
    (add-triple g "b" "next" "c")
    (add-triple g "c" "next" "a")
    (let ((ranks (pagerank g "next")))
      ;; All ranks should be approximately equal
      (let ((vals (mapcar #'cdr ranks)))
        (is (< (- (apply #'max vals) (apply #'min vals)) 0.01))))))

(test pagerank-sum-to-one
  "PageRank values sum to approximately 1.0"
  (let ((g (make-graph)))
    (add-triple g "a" "links" "b")
    (add-triple g "b" "links" "c")
    (add-triple g "c" "links" "a")
    (add-triple g "a" "links" "c")
    (let ((ranks (pagerank g "links")))
      (let ((total (reduce #'+ (mapcar #'cdr ranks))))
        (is (< (abs (- total 1.0)) 0.01))))))

(test pagerank-iterations
  "PageRank accepts custom iteration count"
  (let ((g (make-graph)))
    (add-triple g "a" "links" "b")
    (add-triple g "b" "links" "a")
    (let ((ranks (pagerank g "links" :iterations 50)))
      (is (= 2 (length ranks))))))

;; =============================================================================
;; Clustering Coefficient
;; =============================================================================

(test clustering-coefficient-complete
  "Clustering coefficient of a complete graph is 1.0"
  (let ((g (make-graph)))
    ;; Complete graph on 4 nodes (bidirectional)
    (dolist (pair '(("a" "b") ("a" "c") ("a" "d")
                    ("b" "c") ("b" "d") ("c" "d")
                    ("b" "a") ("c" "a") ("d" "a")
                    ("c" "b") ("d" "b") ("d" "c")))
      (add-triple g (first pair) "knows" (second pair)))
    (let ((cc (clustering-coefficient g "a" "knows")))
      (is (< (abs (- cc 1.0)) 0.01)))))

(test clustering-coefficient-star
  "Clustering coefficient of center of a star is 0.0"
  (let ((g (make-graph)))
    (add-triple g "center" "knows" "a")
    (add-triple g "center" "knows" "b")
    (add-triple g "center" "knows" "c")
    ;; a, b, c don't know each other
    (let ((cc (clustering-coefficient g "center" "knows")))
      (is (< (abs cc) 0.01)))))
