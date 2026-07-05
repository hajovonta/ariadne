;;;; run-shacl12-w3c.lisp
;;;; Run the W3C SHACL 1.2 Core test suite

(require :asdf)
(asdf:load-system :ariadne-tests)
(in-package :ariadne/tests)

(defvar *shacl12-test-base*
  (merge-pathnames "test-data/shacl/data-shapes/shacl12-test-suite/tests/core/"
                   (asdf:system-source-directory :ariadne)))

(dolist (subdir '("targets/" "node/" "property/" "misc/" "complex/" "path/" "validation-reports/"))
  (let ((dir (merge-pathnames subdir *shacl12-test-base*)))
    (when (probe-file dir)
      (format t "~%=== ~A ===~%" subdir)
      (let ((pass 0) (fail 0) (err 0))
        (dolist (file (sort (directory (merge-pathnames "*.ttl" dir)) #'string< :key #'namestring))
          (unless (string= "manifest" (pathname-name file))
            (let ((name (pathname-name file)))
              (unless (or (and (> (length name) 5) (string= "-data" (subseq name (- (length name) 5))))
                          (and (> (length name) 7) (string= "-shapes" (subseq name (- (length name) 7)))))
                (multiple-value-bind (ok expected actual) (run-shacl-w3c-test file)
                  (cond
                    ((eq expected :error) (incf err) (format t "  ERROR ~A~%" name))
                    (ok (incf pass))
                    (t (incf fail) (format t "  FAIL  ~A (expected ~A got ~A)~%" name expected actual))))))))
        (format t "  ~A pass, ~A fail, ~A errors~%" pass fail err)))))
