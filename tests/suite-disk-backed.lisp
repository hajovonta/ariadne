;;;; tests/suite-disk-backed.lisp
;;;; Tests for disk-backed persistence

(in-package #:ariadne/tests)

(in-suite :disk-backed)

(defun tmp-db-path ()
  (merge-pathnames (format nil "test-disk-~A.db" (get-universal-time))
                   #P"/tmp/"))

(test open-and-close-disk-graph
  "open-disk-graph creates a persistent graph, close-disk-graph closes it"
  (let ((path (tmp-db-path)))
    (unwind-protect
         (let ((g (open-disk-graph path :name "disk-test")))
           (is (graphp g))
           (is (= 0 (triple-count g)))
           (close-disk-graph g))
      (ignore-errors (delete-file path)))))

(test disk-graph-add-and-query
  "Triples added to disk graph are queryable"
  (let ((path (tmp-db-path)))
    (unwind-protect
         (let ((g (open-disk-graph path)))
           (add-triple g "http://ex.org/a" "http://ex.org/b" "http://ex.org/c")
           (add-triple g "http://ex.org/d" "http://ex.org/e" "http://ex.org/f")
           (is (= 2 (triple-count g)))
           (is (has-triple-p g "http://ex.org/a" "http://ex.org/b" "http://ex.org/c"))
           (close-disk-graph g))
      (ignore-errors (delete-file path)))))

(test disk-graph-survives-reopen
  "Triples persist across close and reopen"
  (let ((path (tmp-db-path)))
    (unwind-protect
         (progn
           (let ((g (open-disk-graph path :name "persist")))
             (add-triple g "http://ex.org/alice" "http://ex.org/knows" "http://ex.org/bob")
             (add-triple g "http://ex.org/alice" "http://ex.org/age" "30")
             (close-disk-graph g))
           ;; Reopen
           (let ((g (open-disk-graph path)))
             (is (= 2 (triple-count g)))
             (is (has-triple-p g "http://ex.org/alice" "http://ex.org/knows" "http://ex.org/bob"))
             (is (has-triple-p g "http://ex.org/alice" "http://ex.org/age" "30"))
             (close-disk-graph g)))
      (ignore-errors (delete-file path)))))

(test disk-graph-remove-persists
  "Removed triples stay removed after reopen"
  (let ((path (tmp-db-path)))
    (unwind-protect
         (progn
           (let ((g (open-disk-graph path)))
             (add-triple g "http://ex.org/a" "http://ex.org/b" "http://ex.org/c")
             (add-triple g "http://ex.org/d" "http://ex.org/e" "http://ex.org/f")
             (remove-triple g "http://ex.org/a" "http://ex.org/b" "http://ex.org/c")
             (close-disk-graph g))
           (let ((g (open-disk-graph path)))
             (is (= 1 (triple-count g)))
             (is-false (has-triple-p g "http://ex.org/a" "http://ex.org/b" "http://ex.org/c"))
             (is (has-triple-p g "http://ex.org/d" "http://ex.org/e" "http://ex.org/f"))
             (close-disk-graph g)))
      (ignore-errors (delete-file path)))))

(test disk-graph-compact
  "compact-disk-graph removes deleted entries from the file"
  (let ((path (tmp-db-path)))
    (unwind-protect
         (progn
           (let ((g (open-disk-graph path)))
             (dotimes (i 100)
               (add-triple g (format nil "http://ex.org/s~A" i) "http://ex.org/p" "http://ex.org/o"))
             (dotimes (i 50)
               (remove-triple g (format nil "http://ex.org/s~A" i) "http://ex.org/p" "http://ex.org/o"))
             (let ((size-before (file-length (getf (ariadne::graph-extra g) :disk-stream))))
               (compact-disk-graph g)
               (let ((size-after (file-length (getf (ariadne::graph-extra g) :disk-stream))))
                 (is (< size-after size-before))))
             (is (= 50 (triple-count g)))
             (close-disk-graph g))
           ;; Verify after reopen
           (let ((g (open-disk-graph path)))
             (is (= 50 (triple-count g)))
             (close-disk-graph g)))
      (ignore-errors (delete-file path)))))
