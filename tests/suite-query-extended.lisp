;;;; tests/suite-query-extended.lisp
;;;; Extended query features: HAVING, inverse paths, Kleene star, DESCRIBE, REGEX

(in-package #:ariadne/tests)
(in-suite :query-extended)

;; =============================================================================
;; HAVING (filter on aggregated results)
;; =============================================================================

(test having-count
  "HAVING filters groups by aggregated value"
  (let ((g (make-graph)))
    (add-triple g "alice" "works-at" "acme")
    (add-triple g "bob" "works-at" "acme")
    (add-triple g "charlie" "works-at" "acme")
    (add-triple g "dave" "works-at" "globex")
    ;; Only companies with more than 1 employee
    (let ((results (query g '(select (?company (count ?person))
                              (where (?person "works-at" ?company))
                              (group-by ?company)
                              (having (> (count ?person) 1))))))
      (is (= 1 (length results)))
      (is (equal "acme" (first (first results)))))))

(test having-sum
  "HAVING with SUM aggregation"
  (let ((g (make-graph)))
    (add-triple g "alice" "dept" "eng")
    (add-triple g "alice" "salary" 80000)
    (add-triple g "bob" "dept" "eng")
    (add-triple g "bob" "salary" 90000)
    (add-triple g "charlie" "dept" "sales")
    (add-triple g "charlie" "salary" 50000)
    ;; Only departments with total salary > 100000
    (let ((results (query g '(select (?dept (sum ?sal))
                              (where (?person "dept" ?dept)
                                     (?person "salary" ?sal))
                              (group-by ?dept)
                              (having (> (sum ?sal) 100000))))))
      (is (= 1 (length results)))
      (is (equal "eng" (first (first results)))))))

(test having-avg
  "HAVING with AVG"
  (let ((g (make-graph)))
    (add-triple g "alice" "team" "alpha")
    (add-triple g "alice" "score" 90)
    (add-triple g "bob" "team" "alpha")
    (add-triple g "bob" "score" 80)
    (add-triple g "charlie" "team" "beta")
    (add-triple g "charlie" "score" 50)
    (add-triple g "dave" "team" "beta")
    (add-triple g "dave" "score" 40)
    ;; Teams with average score > 60
    (let ((results (query g '(select (?team (avg ?score))
                              (where (?person "team" ?team)
                                     (?person "score" ?score))
                              (group-by ?team)
                              (having (> (avg ?score) 60))))))
      (is (= 1 (length results)))
      (is (equal "alpha" (first (first results)))))))

;; =============================================================================
;; Inverse Paths (^)
;; =============================================================================

(test path-inverse-basic
  "Inverse path: follow edges backwards"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "charlie" "knows" "bob")
    ;; Who is known by bob? (inverse of "knows")
    (let ((results (query g '(select (?who)
                              (where ("bob" (inv "knows") ?who))))))
      (is (= 2 (length results)))
      (is-true (find "alice" results :key #'car :test #'equal))
      (is-true (find "charlie" results :key #'car :test #'equal)))))

(test path-inverse-in-join
  "Inverse path combined with regular patterns"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "charlie" "knows" "bob")
    (add-triple g "alice" "age" 30)
    (add-triple g "charlie" "age" 25)
    ;; People who know bob, with their age
    (let ((results (query g '(select (?who ?age)
                              (where ("bob" (inv "knows") ?who)
                                     (?who "age" ?age))))))
      (is (= 2 (length results))))))

(test path-inverse-transitive
  "Inverse transitive path"
  (let ((g (make-graph)))
    (add-triple g "a" "parent" "b")
    (add-triple g "b" "parent" "c")
    (add-triple g "c" "parent" "d")
    ;; All ancestors of d (inverse transitive parent)
    (let ((results (query g '(select (?ancestor)
                              (where ("d" (inv+ "parent") ?ancestor))))))
      (is (= 3 (length results)))
      (is-true (find "c" results :key #'car :test #'equal))
      (is-true (find "b" results :key #'car :test #'equal))
      (is-true (find "a" results :key #'car :test #'equal)))))

;; =============================================================================
;; Kleene Star (zero or more)
;; =============================================================================

(test path-kleene-star
  "Kleene star: zero or more hops"
  (let ((g (make-graph)))
    (add-triple g "a" "next" "b")
    (add-triple g "b" "next" "c")
    (add-triple g "c" "next" "d")
    ;; Zero or more hops from a — includes a itself
    (let ((results (query g '(select (?node)
                              (where ("a" (* "next") ?node))))))
      (is (= 4 (length results)))
      (is-true (find "a" results :key #'car :test #'equal))
      (is-true (find "d" results :key #'car :test #'equal)))))

(test path-kleene-star-no-edges
  "Kleene star on node with no outgoing edges still returns the node itself"
  (let ((g (make-graph)))
    (add-triple g "lonely" "type" "person")
    (let ((results (query g '(select (?node)
                              (where ("lonely" (* "knows") ?node))))))
      (is (= 1 (length results)))
      (is (equal "lonely" (caar results))))))

(test path-kleene-star-cycle
  "Kleene star handles cycles"
  (let ((g (make-graph)))
    (add-triple g "a" "next" "b")
    (add-triple g "b" "next" "c")
    (add-triple g "c" "next" "a")
    (let ((results (query g '(select (?node)
                              (where ("a" (* "next") ?node))))))
      ;; a, b, c — no duplicates, no infinite loop
      (is (= 3 (length results))))))

;; =============================================================================
;; DESCRIBE
;; =============================================================================

(test describe-basic
  "DESCRIBE returns all triples about a resource"
  (let ((g (make-graph)))
    (add-triple g "alice" "name" "Alice")
    (add-triple g "alice" "age" 30)
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "alice")
    (let ((results (query g '(describe "alice"))))
      ;; Should include triples where alice is subject or object
      (is (= 4 (length results))))))

(test describe-subject-only
  "DESCRIBE with :subject returns only outgoing triples"
  (let ((g (make-graph)))
    (add-triple g "alice" "name" "Alice")
    (add-triple g "alice" "age" 30)
    (add-triple g "bob" "knows" "alice")
    (let ((results (query g '(describe "alice" :subject))))
      (is (= 2 (length results))))))

(test describe-nonexistent
  "DESCRIBE on nonexistent resource returns empty"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (let ((results (query g '(describe "nobody"))))
      (is (= 0 (length results))))))

;; =============================================================================
;; REGEX Filter
;; =============================================================================

(test filter-regex-basic
  "REGEX filter matches patterns"
  (let ((g (make-graph)))
    (add-triple g "alice" "email" "alice@example.com")
    (add-triple g "bob" "email" "bob@test.org")
    (add-triple g "charlie" "email" "charlie@example.com")
    (let ((results (query g '(select (?person)
                              (where (?person "email" ?email))
                              (filter (regex ?email "example\\.com$"))))))
      (is (= 2 (length results))))))

(test filter-regex-case-insensitive
  "REGEX filter with case-insensitive flag"
  (let ((g (make-graph)))
    (add-triple g "alice" "name" "Alice Smith")
    (add-triple g "bob" "name" "Bob SMITH")
    (add-triple g "charlie" "name" "Charlie Jones")
    (let ((results (query g '(select (?person)
                              (where (?person "name" ?name))
                              (filter (regex ?name "smith" :case-insensitive-mode))))))
      (is (= 2 (length results))))))

(test filter-regex-no-match
  "REGEX filter with no matches returns empty"
  (let ((g (make-graph)))
    (add-triple g "alice" "name" "Alice")
    (let ((results (query g '(select (?person)
                              (where (?person "name" ?name))
                              (filter (regex ?name "^Z"))))))
      (is (= 0 (length results))))))
