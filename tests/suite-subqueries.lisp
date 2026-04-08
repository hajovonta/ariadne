;;;; tests/suite-subqueries.lisp
;;;; Subqueries and VALUES inline data

(in-package #:ariadne/tests)
(in-suite :subqueries)

;; =============================================================================
;; VALUES (inline data)
;; =============================================================================

(test values-single-variable
  "VALUES binds a single variable to a set of values"
  (let ((g (make-graph)))
    (add-triple g "alice" "age" 30)
    (add-triple g "bob" "age" 25)
    (add-triple g "charlie" "age" 35)
    (let ((results (query g '(select (?person ?age)
                              (where (?person "age" ?age))
                              (values ?person ("alice" "charlie"))))))
      (is (= 2 (length results)))
      (is-false (find "bob" results :key #'car :test #'equal)))))

(test values-multiple-variables
  "VALUES binds multiple variables"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (add-triple g "bob" "knows" "dave")
    (let ((results (query g '(select (?a ?b)
                              (where (?a "knows" ?b))
                              (values (?a ?b)
                                      (("alice" "bob")
                                       ("bob" "dave")))))))
      (is (= 2 (length results)))
      (is-false (find "charlie" results :key #'second :test #'equal)))))

(test values-as-filter
  "VALUES acts as a filter on query results"
  (let ((g (make-graph)))
    (add-triple g "alice" "type" "person")
    (add-triple g "bob" "type" "person")
    (add-triple g "charlie" "type" "person")
    (add-triple g "dave" "type" "person")
    ;; Only interested in alice and dave
    (let ((results (query g '(select (?person)
                              (where (?person "type" "person"))
                              (values ?person ("alice" "dave"))))))
      (is (= 2 (length results))))))

;; =============================================================================
;; Subqueries
;; =============================================================================

(test subquery-basic
  "Subquery in WHERE clause"
  (let ((g (make-graph)))
    (add-triple g "alice" "age" 30)
    (add-triple g "bob" "age" 25)
    (add-triple g "charlie" "age" 35)
    ;; Find the person with the maximum age
    (let ((results (query g '(select (?person ?age)
                              (where (?person "age" ?age)
                                     (subquery (select ((max ?a))
                                                (where (?anyone "age" ?a)))
                                               ?age))))))
      (is (= 1 (length results)))
      (is (equal "charlie" (first (first results)))))))

(test subquery-exists-pattern
  "Subquery as existence check within WHERE"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (add-triple g "charlie" "type" "person")
    (add-triple g "alice" "type" "person")
    (add-triple g "bob" "type" "person")
    ;; People who know someone who knows charlie
    (let ((results (query g '(select (?person)
                              (where (?person "type" "person")
                                     (subquery (select (?p)
                                                (where (?p "knows" ?mid)
                                                       (?mid "knows" "charlie")))
                                               ?person))))))
      (is (= 1 (length results)))
      (is (equal "alice" (caar results))))))

(test subquery-in-filter
  "Subquery result used in filter"
  (let ((g (make-graph)))
    (add-triple g "alice" "salary" 80000)
    (add-triple g "bob" "salary" 60000)
    (add-triple g "charlie" "salary" 90000)
    ;; People with above-average salary
    (let ((results (query g '(select (?person ?sal)
                              (where (?person "salary" ?sal))
                              (filter (> ?sal
                                        (subquery (select ((avg ?s))
                                                   (where (?x "salary" ?s))))))))))
      (is (= 2 (length results)))
      (is-false (find "bob" results :key #'car :test #'equal)))))
