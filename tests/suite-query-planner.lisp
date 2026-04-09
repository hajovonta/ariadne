;;;; tests/suite-query-planner.lisp
;;;; Query planner: reorder WHERE patterns for optimal execution

(in-package #:ariadne/tests)
(in-suite :query-planner)

;; =============================================================================
;; Selectivity-based reordering
;; =============================================================================

(test planner-bound-subject-first
  "Patterns with bound subjects should execute before fully unbound patterns"
  (let ((g (make-graph)))
    ;; Create a large graph
    (dotimes (i 100)
      (add-triple g (format nil "person-~A" i) "type" "person")
      (add-triple g (format nil "person-~A" i) "age" (+ 20 (mod i 50))))
    (add-triple g "person-0" "knows" "person-1")
    ;; This query should be fast regardless of pattern order
    ;; because the planner puts the bound-subject pattern first
    (let ((results (query g '(select (?age)
                              (where (?x "type" "person")
                                     ("person-0" "knows" ?x)
                                     (?x "age" ?age))))))
      (is (= 1 (length results))))))

(test planner-most-selective-first
  "More selective patterns (more bound positions) execute first"
  (let ((g (make-graph)))
    (dotimes (i 50)
      (add-triple g (format nil "node-~A" i) "connects" (format nil "node-~A" (1+ i)))
      (add-triple g (format nil "node-~A" i) "label" (format nil "label-~A" i)))
    ;; Pattern with bound subject should run first even if listed second
    (let ((results (query g '(select (?label)
                              (where (?x "connects" "node-26")
                                     (?x "label" ?label))))))
      (is (= 1 (length results)))
      (is (equal "label-25" (caar results))))))

(test planner-preserves-correctness
  "Reordering doesn't change query results"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (add-triple g "charlie" "age" 35)
    ;; Results should be the same regardless of internal pattern order
    (let ((r1 (query g '(select (?age)
                          (where ("alice" "knows" ?f)
                                 (?f "knows" ?fof)
                                 (?fof "age" ?age)))))
          (r2 (query g '(select (?age)
                          (where (?fof "age" ?age)
                                 (?f "knows" ?fof)
                                 ("alice" "knows" ?f))))))
      (is (equal (sort (copy-list r1) #'< :key #'car)
                 (sort (copy-list r2) #'< :key #'car))))))

;; =============================================================================
;; Thread Safety
;; =============================================================================

#+bordeaux-threads
(test concurrent-reads
  "Multiple threads can read the graph concurrently"
  (let ((g (make-graph)))
    (dotimes (i 100)
      (add-triple g (format nil "node-~A" i) "type" "node"))
    (let ((results (make-array 4 :initial-element nil)))
      (let ((threads
              (loop for i below 4
                    collect (bt:make-thread
                             (lambda ()
                               (setf (aref results i)
                                     (length (get-triples g :predicate "type"))))))))
        (mapc #'bt:join-thread threads))
      (is (every (lambda (r) (= 100 r)) results)))))
