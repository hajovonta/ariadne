;;;; tests/suite-profiling.lisp
;;;; Query profiling and graph statistics

(in-package #:ariadne/tests)
(in-suite :profiling)

(test profile-query
  "Profile a query and get timing info"
  (let ((g (make-graph)))
    (dotimes (i 100)
      (add-triple g (format nil "node-~A" i) "type" "node"))
    (let ((result (profile-query g '(select (?s) (where (?s "type" "node"))))))
      (is (stringp result))
      (is (search "time" result))
      (is (search "results" result)))))

(test graph-statistics
  "Get graph statistics"
  (let ((g (make-graph :name "test")))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (add-triple g "bob" "age" 25)
    (let ((stats (graph-statistics g)))
      (is (= 3 (getf stats :triples)))
      (is (= 2 (getf stats :subjects)))
      (is (= 2 (getf stats :predicates)))
      (is (= 3 (getf stats :objects))))))

(test index-statistics
  "Get index size statistics"
  (let ((g (make-graph)))
    (dotimes (i 50)
      (add-triple g (format nil "s~A" i) "p" (format nil "o~A" i)))
    (let ((stats (graph-statistics g)))
      (is (= 50 (getf stats :triples)))
      (is (= 50 (getf stats :subjects))))))
