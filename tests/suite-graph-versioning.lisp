;;;; tests/suite-graph-versioning.lisp
;;;; Tests for graph versioning and temporal queries

(in-package #:ariadne/tests)

(in-suite :graph-versioning)

(test create-version
  "graph-checkpoint creates a named version"
  (let ((g (make-graph :name "ver-test")))
    (add-triple g "http://ex.org/a" "http://ex.org/b" "http://ex.org/c")
    (graph-checkpoint g "v1")
    (is (= 1 (length (graph-versions g))))))

(test restore-version
  "graph-restore brings back a previous state"
  (let ((g (make-graph :name "ver-restore")))
    (add-triple g "http://ex.org/a" "http://ex.org/p" "1")
    (graph-checkpoint g "v1")
    (add-triple g "http://ex.org/a" "http://ex.org/p" "2")
    (is (= 2 (triple-count g)))
    (graph-restore g "v1")
    (is (= 1 (triple-count g)))
    (is (has-triple-p g "http://ex.org/a" "http://ex.org/p" "1"))))

(test multiple-versions
  "Multiple checkpoints can be created and restored"
  (let ((g (make-graph :name "ver-multi")))
    (add-triple g "http://ex.org/a" "http://ex.org/p" "1")
    (graph-checkpoint g "v1")
    (add-triple g "http://ex.org/a" "http://ex.org/p" "2")
    (graph-checkpoint g "v2")
    (add-triple g "http://ex.org/a" "http://ex.org/p" "3")
    (graph-checkpoint g "v3")
    (graph-restore g "v1")
    (is (= 1 (triple-count g)))
    (graph-restore g "v2")
    (is (= 2 (triple-count g)))
    (graph-restore g "v3")
    (is (= 3 (triple-count g)))))

(test version-list
  "graph-versions returns version names with timestamps"
  (let ((g (make-graph :name "ver-list")))
    (add-triple g "http://ex.org/a" "http://ex.org/p" "1")
    (graph-checkpoint g "v1")
    (graph-checkpoint g "v2")
    (let ((versions (graph-versions g)))
      (is (= 2 (length versions)))
      (is (string= "v1" (getf (first versions) :name)))
      (is (string= "v2" (getf (second versions) :name)))
      (is (numberp (getf (first versions) :timestamp))))))

(test query-at-version
  "query-at-version runs a query against a historical snapshot"
  (let ((g (make-graph :name "ver-query")))
    (add-triple g "http://ex.org/alice" "http://ex.org/age" "30")
    (graph-checkpoint g "before")
    (add-triple g "http://ex.org/bob" "http://ex.org/age" "25")
    (let ((results (query-at-version g "before"
                     '(select (?s ?o) (where (?s "http://ex.org/age" ?o))))))
      (is (= 1 (length results)))
      (is (string= "http://ex.org/alice" (first (first results)))))))

(test restore-nonexistent-version
  "graph-restore signals error for unknown version"
  (let ((g (make-graph :name "ver-noexist")))
    (signals error (graph-restore g "nope"))))
