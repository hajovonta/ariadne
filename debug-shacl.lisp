(require :asdf)
(asdf:load-system :ariadne)
(in-package :ariadne)
(let ((g (make-graph :name "test")))
  (import-turtle g (with-open-file (s "test-data/shacl/data-shapes/data-shapes-test-suite/tests/core/property/or-001.ttl")
                     (let ((b (make-string (file-length s)))) (read-sequence b s) b)))
  ;; Check what shapes are found
  (format t "Shapes: ~S~%" (find-shapes g))
  ;; Check targets for AddressShape
  (let ((shape "http://datashapes.org/sh/tests/core/property/or-001.test#AddressShape"))
    (format t "Targets: ~S~%" (shape-targets g shape))
    (format t "Prop shapes: ~S~%" (shape-property-shapes g shape))
    ;; Check the property shape
    (let ((ps "http://datashapes.org/sh/tests/core/property/or-001.test#AddressShape-address"))
      (format t "Path: ~S~%" (prop-shape-path g ps))
      (format t "or list: ~S~%" (prop-shape-list-value g ps "or"))
      ;; Check values for InvalidResource1
      (let ((focus "http://datashapes.org/sh/tests/core/property/or-001.test#InvalidResource1"))
        (format t "Values for ~A: ~S~%" focus
                (resolve-path-values g focus (prop-shape-path g ps)))
        (format t "Violations: ~S~%" (check-property-shape g focus ps shape))))))
