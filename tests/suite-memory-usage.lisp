;;;; tests/suite-memory-usage.lisp
;;;; Tests for memory usage reporting

(in-package #:ariadne/tests)

(in-suite :memory-usage)

(test memory-usage-empty-graph
  "Memory usage on empty graph returns expected keys"
  (let* ((g (make-graph :name "empty"))
         (report (graph-memory-usage g)))
    (is (listp report))
    (is (numberp (getf report :index-bytes)))
    (is (numberp (getf report :triple-count)))
    (is (= 0 (getf report :triple-count)))))

(test memory-usage-with-data
  "Memory usage grows with triples"
  (let ((g (make-graph :name "mem-test")))
    (let ((before (getf (graph-memory-usage g) :index-bytes)))
      (dotimes (i 100)
        (add-triple g
                    (format nil "http://ex.org/s~A" i)
                    "http://ex.org/p"
                    (format nil "http://ex.org/o~A" i)))
      (let ((after (getf (graph-memory-usage g) :index-bytes)))
        (is (= 100 (getf (graph-memory-usage g) :triple-count)))
        (is (> after before))))))

(test memory-usage-intern-table
  "Memory usage reports intern table size"
  (let* ((g (make-graph :name "intern-test"))
         (report (graph-memory-usage g)))
    (is (numberp (getf report :intern-count)))))
