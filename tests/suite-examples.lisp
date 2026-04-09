;;;; tests/suite-examples.lisp
;;;; Example datasets and queries

(in-package #:ariadne/tests)
(in-suite :examples)

;; =============================================================================
;; Example Dataset Loading
;; =============================================================================

(test example-family-tree-loads
  "Family tree example loads without error"
  (let ((path (merge-pathnames "examples/family-tree.ttl"
                               (asdf:system-source-directory :ariadne-tests))))
    (when (probe-file path)
      (let ((g (make-graph)))
        (import-turtle g (uiop:read-file-string path))
        (is (> (triple-count g) 10))))))

(test example-family-tree-queries
  "Family tree example queries work"
  (let ((path (merge-pathnames "examples/family-tree.ttl"
                               (asdf:system-source-directory :ariadne-tests))))
    (when (probe-file path)
      (let ((g (make-graph)))
        (import-turtle g (uiop:read-file-string path))
        ;; Find all people
        (let ((people (query g '(select (?person)
                                 (where (?person "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                                                 "http://example.org/family#Person"))))))
          (is (> (length people) 0)))
        ;; Find parent-child relationships
        (let ((parents (query g '(select (?parent ?child)
                                  (where (?parent "http://example.org/family#hasChild" ?child))))))
          (is (> (length parents) 0)))
        ;; Transitive ancestors
        (let ((ancestors (query g '(select (?ancestor)
                                    (where ("http://example.org/family#alice"
                                            (+ "http://example.org/family#hasChild")
                                            ?ancestor))))))
          (is (> (length ancestors) 0)))))))
