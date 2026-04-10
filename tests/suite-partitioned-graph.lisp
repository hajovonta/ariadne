;;;; tests/suite-partitioned-graph.lisp
;;;; Tests for graph partitioning

(in-package #:ariadne/tests)

(in-suite :partitioned-graph)

(test make-partitioned-graph
  "Create a partitioned graph with N partitions"
  (let ((pg (make-partitioned-graph :name "part-test" :partitions 4)))
    (is (= 4 (partition-count pg)))))

(test partitioned-add-and-query
  "Triples added to partitioned graph are queryable"
  (let ((pg (make-partitioned-graph :partitions 4)))
    (add-triple pg "http://ex.org/a" "http://ex.org/p" "http://ex.org/b")
    (add-triple pg "http://ex.org/c" "http://ex.org/p" "http://ex.org/d")
    (is (= 2 (triple-count pg)))
    (is (has-triple-p pg "http://ex.org/a" "http://ex.org/p" "http://ex.org/b"))
    (is (has-triple-p pg "http://ex.org/c" "http://ex.org/p" "http://ex.org/d"))))

(test partitioned-get-triples
  "get-triples works across partitions"
  (let ((pg (make-partitioned-graph :partitions 4)))
    (add-triple pg "http://ex.org/a" "http://ex.org/type" "Person")
    (add-triple pg "http://ex.org/b" "http://ex.org/type" "Person")
    (add-triple pg "http://ex.org/c" "http://ex.org/type" "Animal")
    (is (= 2 (length (get-triples pg :object "Person"))))
    (is (= 3 (length (get-triples pg))))))

(test partitioned-remove
  "remove-triple works on partitioned graph"
  (let ((pg (make-partitioned-graph :partitions 4)))
    (add-triple pg "http://ex.org/a" "http://ex.org/p" "http://ex.org/b")
    (remove-triple pg "http://ex.org/a" "http://ex.org/p" "http://ex.org/b")
    (is (= 0 (triple-count pg)))))

(test partitioned-distributes
  "Triples are distributed across partitions"
  (let ((pg (make-partitioned-graph :partitions 4)))
    (dotimes (i 100)
      (add-triple pg (format nil "http://ex.org/s~A" i) "http://ex.org/p" "http://ex.org/o"))
    (is (= 100 (triple-count pg)))
    ;; At least 2 partitions should have data (probabilistic but safe with 100 items)
    (let ((non-empty (count-if (lambda (g) (> (triple-count g) 0))
                               (graph-partitions pg))))
      (is (>= non-empty 2)))))

(test partitioned-query
  "query works on partitioned graph"
  (let ((pg (make-partitioned-graph :partitions 4)))
    (add-triple pg "http://ex.org/alice" "http://ex.org/knows" "http://ex.org/bob")
    (add-triple pg "http://ex.org/bob" "http://ex.org/knows" "http://ex.org/carol")
    (let ((results (query pg '(select (?s ?o) (where (?s "http://ex.org/knows" ?o))))))
      (is (= 2 (length results))))))
