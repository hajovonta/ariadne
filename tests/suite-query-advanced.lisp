;;;; tests/suite-query-advanced.lisp
;;;; Advanced query features: GROUP BY, aggregations, ASK, CONSTRUCT, BIND, NOT EXISTS

(in-package #:ariadne/tests)
(in-suite :query-advanced)

;; =============================================================================
;; ASK Queries
;; =============================================================================

(test ask-true
  "ASK returns T when pattern matches"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (is-true (query g '(ask (where ("alice" "knows" "bob")))))))

(test ask-false
  "ASK returns NIL when pattern doesn't match"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (is-false (query g '(ask (where ("alice" "knows" "charlie")))))))

(test ask-with-variables
  "ASK with variables — true if any binding exists"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (is-true (query g '(ask (where ("alice" "knows" ?x)))))))

(test ask-with-join
  "ASK with multi-pattern join"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "age" 25)
    (is-true (query g '(ask (where ("alice" "knows" ?friend)
                                   (?friend "age" ?age)))))))

;; =============================================================================
;; GROUP BY
;; =============================================================================

(test group-by-single
  "Group by a single variable"
  (let ((g (make-graph)))
    (add-triple g "alice" "works-at" "acme")
    (add-triple g "bob" "works-at" "acme")
    (add-triple g "charlie" "works-at" "globex")
    (let ((results (query g '(select (?company (count ?person))
                              (where (?person "works-at" ?company))
                              (group-by ?company)))))
      (is (= 2 (length results)))
      ;; acme should have count 2
      (let ((acme (find "acme" results :key #'first :test #'equal)))
        (is (= 2 (second acme))))
      ;; globex should have count 1
      (let ((globex (find "globex" results :key #'first :test #'equal)))
        (is (= 1 (second globex)))))))

(test group-by-with-sum
  "Group by with SUM aggregation"
  (let ((g (make-graph)))
    (add-triple g "alice" "department" "engineering")
    (add-triple g "alice" "salary" 80000)
    (add-triple g "bob" "department" "engineering")
    (add-triple g "bob" "salary" 90000)
    (add-triple g "charlie" "department" "sales")
    (add-triple g "charlie" "salary" 70000)
    (let ((results (query g '(select (?dept (sum ?sal))
                              (where (?person "department" ?dept)
                                     (?person "salary" ?sal))
                              (group-by ?dept)))))
      (let ((eng (find "engineering" results :key #'first :test #'equal)))
        (is (= 170000 (second eng)))))))

(test group-by-with-avg
  "Group by with AVG aggregation"
  (let ((g (make-graph)))
    (add-triple g "alice" "department" "engineering")
    (add-triple g "alice" "salary" 80000)
    (add-triple g "bob" "department" "engineering")
    (add-triple g "bob" "salary" 90000)
    (let ((results (query g '(select (?dept (avg ?sal))
                              (where (?person "department" ?dept)
                                     (?person "salary" ?sal))
                              (group-by ?dept)))))
      (let ((eng (find "engineering" results :key #'first :test #'equal)))
        (is (= 85000 (second eng)))))))

(test group-by-with-min-max
  "Group by with MIN and MAX aggregation"
  (let ((g (make-graph)))
    (add-triple g "alice" "team" "alpha")
    (add-triple g "alice" "score" 90)
    (add-triple g "bob" "team" "alpha")
    (add-triple g "bob" "score" 70)
    (add-triple g "charlie" "team" "alpha")
    (add-triple g "charlie" "score" 85)
    (let ((results (query g '(select (?team (min ?score) (max ?score))
                              (where (?person "team" ?team)
                                     (?person "score" ?score))
                              (group-by ?team)))))
      (let ((alpha (find "alpha" results :key #'first :test #'equal)))
        (is (= 70 (second alpha)))
        (is (= 90 (third alpha)))))))

;; =============================================================================
;; BIND
;; =============================================================================

(test bind-computed-value
  "BIND assigns a computed value to a new variable"
  (let ((g (make-graph)))
    (add-triple g "alice" "birth-year" 1990)
    (let ((results (query g '(select (?person ?age)
                              (where (?person "birth-year" ?year))
                              (bind ?age (- 2026 ?year))))))
      (is (= 1 (length results)))
      (is (= 36 (second (first results)))))))

(test bind-string-concat
  "BIND with string concatenation"
  (let ((g (make-graph)))
    (add-triple g "alice" "first-name" "Alice")
    (add-triple g "alice" "last-name" "Smith")
    (let ((results (query g '(select (?person ?full)
                              (where (?person "first-name" ?first)
                                     (?person "last-name" ?last))
                              (bind ?full (concatenate 'string ?first " " ?last))))))
      (is (equal "Alice Smith" (second (first results)))))))

(test bind-multiple
  "Multiple BIND clauses"
  (let ((g (make-graph)))
    (add-triple g "rect" "width" 10)
    (add-triple g "rect" "height" 5)
    (let ((results (query g '(select (?shape ?area ?perimeter)
                              (where (?shape "width" ?w)
                                     (?shape "height" ?h))
                              (bind ?area (* ?w ?h))
                              (bind ?perimeter (* 2 (+ ?w ?h)))))))
      (is (= 50 (second (first results))))
      (is (= 30 (third (first results)))))))

;; =============================================================================
;; NOT EXISTS / MINUS
;; =============================================================================

(test not-exists-basic
  "NOT EXISTS excludes results where pattern matches"
  (let ((g (make-graph)))
    (add-triple g "alice" "type" "person")
    (add-triple g "bob" "type" "person")
    (add-triple g "charlie" "type" "person")
    (add-triple g "alice" "email" "alice@example.com")
    (add-triple g "bob" "email" "bob@example.com")
    ;; charlie has no email
    (let ((results (query g '(select (?person)
                              (where (?person "type" "person"))
                              (not-exists (?person "email" ?any))))))
      (is (= 1 (length results)))
      (is (equal "charlie" (caar results))))))

(test not-exists-with-join
  "NOT EXISTS with a join pattern"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (add-triple g "charlie" "knows" "dave")
    ;; People who don't know anyone
    (add-triple g "dave" "type" "person")
    (add-triple g "alice" "type" "person")
    (add-triple g "bob" "type" "person")
    (add-triple g "charlie" "type" "person")
    (let ((results (query g '(select (?person)
                              (where (?person "type" "person"))
                              (not-exists (?person "knows" ?anyone))))))
      (is (= 1 (length results)))
      (is (equal "dave" (caar results))))))

(test minus-basic
  "MINUS removes matching bindings"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (add-triple g "alice" "knows" "dave")
    (add-triple g "alice" "dislikes" "dave")
    ;; People alice knows but doesn't dislike
    (let ((results (query g '(select (?who)
                              (where ("alice" "knows" ?who))
                              (minus ("alice" "dislikes" ?who))))))
      (is (= 2 (length results)))
      (is-false (find "dave" results :key #'car :test #'equal)))))

;; =============================================================================
;; CONSTRUCT
;; =============================================================================

(test construct-basic
  "CONSTRUCT creates new triples from query results"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (let ((new-triples (query g '(construct (?a "friend-of-friend" ?c)
                                  (where (?a "knows" ?b)
                                         (?b "knows" ?c))))))
      (is (= 1 (length new-triples)))
      (is (equal "alice" (first (first new-triples))))
      (is (equal "friend-of-friend" (second (first new-triples))))
      (is (equal "charlie" (third (first new-triples)))))))

(test construct-insert
  "CONSTRUCT with :into inserts triples into a target graph"
  (let ((g (make-graph))
        (g2 (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (query g '(construct (?a "friend-of-friend" ?c)
               (where (?a "knows" ?b)
                      (?b "knows" ?c))
               (into g2)))
    ;; g2 should now have the inferred triple
    (is-true (has-triple-p g2 "alice" "friend-of-friend" "charlie"))))

;; =============================================================================
;; Property Paths (Transitive Closure)
;; =============================================================================

(test path-transitive
  "Transitive closure: ?a knows+ ?b"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (add-triple g "charlie" "knows" "dave")
    (let ((results (query g '(select (?reachable)
                              (where ("alice" (+ "knows") ?reachable))))))
      ;; Should find bob, charlie, and dave
      (is (= 3 (length results))))))

(test path-transitive-with-cycle
  "Transitive closure handles cycles"
  (let ((g (make-graph)))
    (add-triple g "a" "next" "b")
    (add-triple g "b" "next" "c")
    (add-triple g "c" "next" "a")
    (let ((results (query g '(select (?node)
                              (where ("a" (+ "next") ?node))))))
      ;; Should find b, c, a (back to start) without infinite loop
      (is (= 3 (length results))))))

(test path-optional-one-or-zero
  "Optional path: ?a knows? ?b (zero or one hop)"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (let ((results (query g '(select (?person)
                              (where ("alice" (? "knows") ?person))))))
      ;; Should find alice (zero hops) and bob (one hop)
      (is (= 2 (length results))))))

(test path-alternative
  "Alternative paths: knows|likes"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "likes" "charlie")
    (add-triple g "alice" "hates" "dave")
    (let ((results (query g '(select (?person)
                              (where ("alice" (alt "knows" "likes") ?person))))))
      (is (= 2 (length results)))
      (is-false (find "dave" results :key #'car :test #'equal)))))

(test path-bounded
  "Bounded path: knows{1,2} (one or two hops)"
  (let ((g (make-graph)))
    (add-triple g "a" "knows" "b")
    (add-triple g "b" "knows" "c")
    (add-triple g "c" "knows" "d")
    (let ((results (query g '(select (?node)
                              (where ("a" (range "knows" 1 2) ?node))))))
      ;; Should find b (1 hop) and c (2 hops) but not d (3 hops)
      (is (= 2 (length results))))))
