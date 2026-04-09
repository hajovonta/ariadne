;;;; tests/suite-streaming.lisp
;;;; Streaming import for large files

(in-package #:ariadne/tests)
(in-suite :streaming-import)

;; =============================================================================
;; Streaming N-Triples
;; =============================================================================

(test stream-import-ntriples-file
  "Stream import N-Triples from file"
  (let ((g (make-graph))
        (path (merge-pathnames "test-data/sample.nt"
                               (asdf:system-source-directory :ariadne-tests))))
    (when (probe-file path)
      (stream-import-ntriples g path)
      (is (> (triple-count g) 0)))))

(test stream-import-ntriples-matches-bulk
  "Streaming import produces same result as bulk import"
  (let ((g1 (make-graph))
        (g2 (make-graph))
        (path (merge-pathnames "test-data/sample.nt"
                               (asdf:system-source-directory :ariadne-tests))))
    (when (probe-file path)
      (import-ntriples g1 (uiop:read-file-string path))
      (stream-import-ntriples g2 path)
      (is (= (triple-count g1) (triple-count g2))))))

;; =============================================================================
;; Streaming N-Quads
;; =============================================================================

(test stream-import-nquads-file
  "Stream import N-Quads from file"
  (let ((g (make-graph))
        (path #p"~/Downloads/bio2rdf-clinicaltrials-R3-statistics.nq"))
    (when (probe-file path)
      (stream-import-nquads g path)
      (is (> (triple-count g) 0)))))
