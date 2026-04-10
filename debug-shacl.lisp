(require :asdf)
(asdf:load-system :ariadne)
(in-package :ariadne)
(let ((g (make-graph :name "test")))
  (import-turtle g (with-open-file (s "test-data/shacl/data-shapes/data-shapes-test-suite/tests/core/path/path-zeroOrMore-001.ttl")
                     (let ((b (make-string (file-length s)))) (read-sequence b s) b)))
  ;; Manually test path resolution
  (let ((focus "http://datashapes.org/sh/tests/core/path/path-zeroOrMore-001.test#InvalidResource1")
        (path "_:anon1"))
    (format t "resolve-path-values: ~S~%" (resolve-path-values g focus path))
    ;; Also test transitive directly
    (format t "transitive (zero-more): ~S~%"
            (transitive-path-values g focus
                                    "http://datashapes.org/sh/tests/core/path/path-zeroOrMore-001.test#child" t))))
