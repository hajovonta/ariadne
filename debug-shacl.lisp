(require :asdf)
(asdf:load-system :ariadne)
(in-package :ariadne)
(handler-case
    (let ((g (make-graph :name "test")))
      (import-turtle g (with-open-file (s "test-data/shacl/data-shapes/data-shapes-test-suite/tests/core/node/datatype-001.ttl")
                         (let ((b (make-string (file-length s)))) (read-sequence b s) b)))
      (format t "Triples: ~A~%" (triple-count g)))
  (error (e) (format t "ERROR: ~A~%" e)))
