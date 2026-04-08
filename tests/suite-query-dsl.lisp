;;;; tests/suite-query-dsl.lisp
;;;; CL query DSL: SPARQL-like select/where/filter

(in-package #:ariadne/tests)
(in-suite :query-dsl)

;; =============================================================================
;; Basic SELECT/WHERE
;; =============================================================================

(test select-single-variable
  "Select a single variable from pattern match"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (let ((results (query g '(select (?who)
                              (where ("alice" "knows" ?who))))))
      (is (= 2 (length results)))
      (is-true (member '(("bob")) results :test #'equal))
      (is-true (member '(("charlie")) results :test #'equal)))))

(test select-multiple-variables
  "Select multiple variables"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "age" 30)
    (let ((results (query g '(select (?pred ?obj)
                              (where ("alice" ?pred ?obj))))))
      (is (= 2 (length results))))))

(test select-with-join
  "Join across multiple triple patterns"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (let ((results (query g '(select (?friend-of-friend)
                              (where ("alice" "knows" ?friend)
                                     (?friend "knows" ?friend-of-friend))))))
      (is (= 1 (length results)))
      (is (equal "charlie" (caar results))))))

(test select-all-variables
  "Select * returns all bound variables"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (let ((results (query g '(select *
                              (where (?s "knows" ?o))))))
      (is (= 1 (length results))))))

;; =============================================================================
;; FILTER
;; =============================================================================

(test filter-numeric-comparison
  "Filter with numeric comparison"
  (let ((g (make-graph)))
    (add-triple g "alice" "age" 30)
    (add-triple g "bob" "age" 25)
    (add-triple g "charlie" "age" 35)
    (let ((results (query g '(select (?person)
                              (where (?person "age" ?age))
                              (filter (> ?age 28))))))
      (is (= 2 (length results))))))

(test filter-string-match
  "Filter with string matching"
  (let ((g (make-graph)))
    (add-triple g "alice" "name" "Alice Smith")
    (add-triple g "bob" "name" "Bob Jones")
    (let ((results (query g '(select (?person)
                              (where (?person "name" ?name))
                              (filter (search "Smith" ?name))))))
      (is (= 1 (length results))))))

(test filter-equality
  "Filter with equality"
  (let ((g (make-graph)))
    (add-triple g "alice" "type" "person")
    (add-triple g "acme" "type" "company")
    (let ((results (query g '(select (?entity)
                              (where (?entity "type" ?type))
                              (filter (equal ?type "person"))))))
      (is (= 1 (length results))))))

(test filter-combined
  "Multiple filter conditions (AND)"
  (let ((g (make-graph)))
    (add-triple g "alice" "age" 30)
    (add-triple g "alice" "type" "person")
    (add-triple g "bob" "age" 25)
    (add-triple g "bob" "type" "person")
    (add-triple g "charlie" "age" 35)
    (add-triple g "charlie" "type" "robot")
    (let ((results (query g '(select (?who)
                              (where (?who "age" ?age)
                                     (?who "type" ?type))
                              (filter (> ?age 28)
                                      (equal ?type "person"))))))
      (is (= 1 (length results))))))

;; =============================================================================
;; OPTIONAL
;; =============================================================================

(test optional-pattern
  "Optional pattern: include results even when optional part doesn't match"
  (let ((g (make-graph)))
    (add-triple g "alice" "name" "Alice")
    (add-triple g "alice" "email" "alice@example.com")
    (add-triple g "bob" "name" "Bob")
    ;; Bob has no email
    (let ((results (query g '(select (?name ?email)
                              (where (?person "name" ?name))
                              (optional (?person "email" ?email))))))
      (is (= 2 (length results))))))

;; =============================================================================
;; UNION
;; =============================================================================

(test union-patterns
  "Union of two patterns"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "charlie" "likes" "bob")
    (let ((results (query g '(select (?who)
                              (union
                               (where (?who "knows" "bob"))
                               (where (?who "likes" "bob")))))))
      (is (= 2 (length results))))))

;; =============================================================================
;; COUNT / AGGREGATE
;; =============================================================================

(test count-results
  "Count matching results"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (add-triple g "alice" "knows" "dave")
    (let ((result (query g '(select ((count ?friend))
                             (where ("alice" "knows" ?friend))))))
      (is (= 3 (caar result))))))

(test distinct-results
  "Select distinct values"
  (let ((g (make-graph)))
    (add-triple g "alice" "type" "person")
    (add-triple g "bob" "type" "person")
    (add-triple g "acme" "type" "company")
    (let ((results (query g '(select-distinct (?type)
                              (where (?x "type" ?type))))))
      (is (= 2 (length results))))))

;; =============================================================================
;; ORDER BY / LIMIT
;; =============================================================================

(test order-by
  "Order results by a variable"
  (let ((g (make-graph)))
    (add-triple g "charlie" "age" 35)
    (add-triple g "alice" "age" 30)
    (add-triple g "bob" "age" 25)
    (let ((results (query g '(select (?person ?age)
                              (where (?person "age" ?age))
                              (order-by ?age)))))
      (is (= 25 (second (first results))))
      (is (= 35 (second (third results)))))))

(test limit-results
  "Limit number of results"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (add-triple g "alice" "knows" "dave")
    (let ((results (query g '(select (?who)
                              (where ("alice" "knows" ?who))
                              (limit 2)))))
      (is (= 2 (length results))))))

(test offset-results
  "Skip results with offset"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (add-triple g "alice" "knows" "dave")
    (let ((results (query g '(select (?who)
                              (where ("alice" "knows" ?who))
                              (order-by ?who)
                              (offset 1)
                              (limit 2)))))
      (is (= 2 (length results))))))
