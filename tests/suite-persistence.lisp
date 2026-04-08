;;;; tests/suite-persistence.lisp
;;;; Save/load graph to/from disk

(in-package #:ariadne/tests)
(in-suite :persistence)

;; =============================================================================
;; Save and Load
;; =============================================================================

(test save-and-load-basic
  "Save a graph to disk and load it back"
  (let ((g (make-graph))
        (path (merge-pathnames "test-data/test-save.ariadne"
                               (asdf:system-source-directory :ariadne-tests))))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "age" 30)
    (save-graph g path)
    (let ((g2 (load-graph path)))
      (is (= 2 (triple-count g2)))
      (is-true (has-triple-p g2 "alice" "knows" "bob"))
      (is-true (has-triple-p g2 "alice" "age" 30)))
    ;; Cleanup
    (when (probe-file path) (delete-file path))))

(test save-and-load-empty-graph
  "Save and load an empty graph"
  (let ((g (make-graph))
        (path (merge-pathnames "test-data/test-empty.ariadne"
                               (asdf:system-source-directory :ariadne-tests))))
    (save-graph g path)
    (let ((g2 (load-graph path)))
      (is (= 0 (triple-count g2))))
    (when (probe-file path) (delete-file path))))

(test save-and-load-preserves-types
  "Save/load preserves data types (strings, numbers, symbols)"
  (let ((g (make-graph))
        (path (merge-pathnames "test-data/test-types.ariadne"
                               (asdf:system-source-directory :ariadne-tests))))
    (add-triple g "alice" "name" "Alice Smith")
    (add-triple g "alice" "age" 30)
    (add-triple g "alice" "height" 1.65)
    (add-triple g :alice :type :person)
    (save-graph g path)
    (let ((g2 (load-graph path)))
      (is (= 4 (triple-count g2)))
      ;; Check types are preserved
      (let ((age-triples (get-triples g2 :subject "alice" :predicate "age")))
        (is (integerp (triple-object (first age-triples)))))
      (let ((height-triples (get-triples g2 :subject "alice" :predicate "height")))
        (is (floatp (triple-object (first height-triples))))))
    (when (probe-file path) (delete-file path))))

(test save-and-load-large-graph
  "Save and load a graph with many triples"
  (let ((g (make-graph))
        (path (merge-pathnames "test-data/test-large.ariadne"
                               (asdf:system-source-directory :ariadne-tests))))
    (dotimes (i 1000)
      (add-triple g (format nil "node-~A" i)
                    "connects"
                    (format nil "node-~A" (1+ i))))
    (save-graph g path)
    (let ((g2 (load-graph path)))
      (is (= 1000 (triple-count g2))))
    (when (probe-file path) (delete-file path))))

;; =============================================================================
;; Named Graph Persistence
;; =============================================================================

(test save-preserves-graph-name
  "Graph name is preserved across save/load"
  (let ((g (make-graph :name "my-knowledge-base"))
        (path (merge-pathnames "test-data/test-named.ariadne"
                               (asdf:system-source-directory :ariadne-tests))))
    (add-triple g "alice" "knows" "bob")
    (save-graph g path)
    (let ((g2 (load-graph path)))
      (is (string= "my-knowledge-base" (graph-name g2))))
    (when (probe-file path) (delete-file path))))
