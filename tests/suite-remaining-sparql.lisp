;;;; tests/suite-remaining-sparql.lisp
;;;; Remaining SPARQL features: sequence paths, GROUP_CONCAT, SAMPLE

(in-package #:ariadne/tests)
(in-suite :remaining-sparql)

;; =============================================================================
;; Sequence Paths (/)
;; =============================================================================

(test path-sequence-two-steps
  "Sequence path: two predicates chained"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "name" "Bob Smith")
    ;; alice knows/name => "Bob Smith"
    (let ((results (query g '(select (?name)
                              (where ("alice" (seq "knows" "name") ?name))))))
      (is (= 1 (length results)))
      (is (equal "Bob Smith" (caar results))))))

(test path-sequence-three-steps
  "Sequence path: three predicates chained"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "works-at" "acme")
    (add-triple g "acme" "location" "NYC")
    (let ((results (query g '(select (?loc)
                              (where ("alice" (seq "knows" "works-at" "location") ?loc))))))
      (is (= 1 (length results)))
      (is (equal "NYC" (caar results))))))

(test path-sequence-multiple-results
  "Sequence path with fan-out"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (add-triple g "bob" "name" "Bob")
    (add-triple g "charlie" "name" "Charlie")
    (let ((results (query g '(select (?name)
                              (where ("alice" (seq "knows" "name") ?name))))))
      (is (= 2 (length results))))))

;; =============================================================================
;; GROUP_CONCAT
;; =============================================================================

(test group-concat-basic
  "GROUP_CONCAT joins values into a string"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (add-triple g "alice" "knows" "dave")
    (let ((results (query g '(select (?person (group-concat ?friend))
                              (where (?person "knows" ?friend))
                              (group-by ?person)))))
      (is (= 1 (length results)))
      (let ((concat-val (second (first results))))
        (is (stringp concat-val))
        (is (search "bob" concat-val))
        (is (search "charlie" concat-val))))))

(test group-concat-separator
  "GROUP_CONCAT with custom separator"
  (let ((g (make-graph)))
    (add-triple g "alice" "tag" "lisp")
    (add-triple g "alice" "tag" "graph")
    (let ((results (query g '(select (?person (group-concat ?tag "; "))
                              (where (?person "tag" ?tag))
                              (group-by ?person)))))
      (let ((concat-val (second (first results))))
        (is (search "; " concat-val))))))

;; =============================================================================
;; SAMPLE
;; =============================================================================

(test sample-returns-one-value
  "SAMPLE returns exactly one value from the group"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (add-triple g "alice" "knows" "dave")
    (let ((results (query g '(select (?person (sample ?friend))
                              (where (?person "knows" ?friend))
                              (group-by ?person)))))
      (is (= 1 (length results)))
      (let ((sampled (second (first results))))
        (is-true (member sampled '("bob" "charlie" "dave") :test #'equal))))))
