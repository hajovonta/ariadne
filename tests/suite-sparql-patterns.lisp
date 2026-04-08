;;;; tests/suite-sparql-patterns.lisp
;;;; SPARQL-like triple pattern matching with logic variables

(in-package #:ariadne/tests)
(in-suite :sparql-patterns)

;; =============================================================================
;; Basic Pattern Matching
;; =============================================================================

(test match-single-pattern
  "Match a single triple pattern with one variable"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (let ((bindings (match-pattern g '("alice" "knows" ?x))))
      (is (= 2 (length bindings)))
      (is-true (find "bob" bindings :key (lambda (b) (cdr (assoc '?x b))) :test #'equal))
      (is-true (find "charlie" bindings :key (lambda (b) (cdr (assoc '?x b))) :test #'equal)))))

(test match-two-variables
  "Match a pattern with two variables"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "age" 30)
    (let ((bindings (match-pattern g '("alice" ?p ?o))))
      (is (= 2 (length bindings)))
      (is-true (find "knows" bindings
                     :key (lambda (b) (cdr (assoc '?p b))) :test #'equal))
      (is-true (find "age" bindings
                     :key (lambda (b) (cdr (assoc '?p b))) :test #'equal)))))

(test match-all-variables
  "Match a pattern with all three positions as variables"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "age" 25)
    (let ((bindings (match-pattern g '(?s ?p ?o))))
      (is (= 2 (length bindings))))))

(test match-no-results
  "Pattern that matches nothing returns empty list"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (let ((bindings (match-pattern g '("nobody" "knows" ?x))))
      (is (= 0 (length bindings))))))

;; =============================================================================
;; Multi-Pattern Joins
;; =============================================================================

(test join-two-patterns
  "Join two patterns sharing a variable"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "age" 25)
    (let ((bindings (match-patterns g '(("alice" "knows" ?person)
                                        (?person "age" ?age)))))
      (is (= 1 (length bindings)))
      (is (equal "bob" (cdr (assoc '?person (first bindings)))))
      (is (= 25 (cdr (assoc '?age (first bindings))))))))

(test join-three-patterns
  "Join three patterns (friend-of-friend with ages)"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (add-triple g "charlie" "age" 35)
    (let ((bindings (match-patterns g '(("alice" "knows" ?friend)
                                        (?friend "knows" ?fof)
                                        (?fof "age" ?age)))))
      (is (= 1 (length bindings)))
      (is (= 35 (cdr (assoc '?age (first bindings))))))))

(test join-with-shared-object
  "Join where two patterns share the same object"
  (let ((g (make-graph)))
    (add-triple g "alice" "works-at" "acme")
    (add-triple g "bob" "works-at" "acme")
    (add-triple g "charlie" "works-at" "globex")
    (let ((bindings (match-patterns g '((?p1 "works-at" ?company)
                                        (?p2 "works-at" ?company)))))
      ;; acme: alice-alice, alice-bob, bob-alice, bob-bob = 4
      ;; globex: charlie-charlie = 1
      ;; total = 5
      (is (= 5 (length bindings)))
      ;; Verify a cross-pair exists (alice, bob at acme)
      (is-true (find-if (lambda (b)
                          (and (equal "alice" (cdr (assoc '?p1 b)))
                               (equal "bob" (cdr (assoc '?p2 b)))))
                        bindings)))))

;; =============================================================================
;; Variable Binding
;; =============================================================================

(test variable-detection
  "Symbols starting with ? are recognized as variables"
  (is-true (variable-p '?x))
  (is-true (variable-p '?person))
  (is-false (variable-p 'alice))
  (is-false (variable-p "alice"))
  (is-false (variable-p 42)))

(test binding-lookup
  "Look up a variable in a binding environment"
  (let ((env '((?x . "alice") (?y . "bob"))))
    (is (equal "alice" (lookup-binding '?x env)))
    (is (equal "bob" (lookup-binding '?y env)))
    (is-false (lookup-binding '?z env))))

;; =============================================================================
;; Unification
;; =============================================================================

(test unify-constant-match
  "Unify a constant with a matching value"
  (multiple-value-bind (env success) (unify "alice" "alice" nil)
    (declare (ignore env))
    (is-true success)))

(test unify-constant-mismatch
  "Unify a constant with a non-matching value fails"
  (multiple-value-bind (env success) (unify "alice" "bob" nil)
    (declare (ignore env))
    (is-false success)))

(test unify-variable-unbound
  "Unify an unbound variable binds it"
  (let ((result (unify '?x "alice" nil)))
    (is (equal "alice" (cdr (assoc '?x result))))))

(test unify-variable-bound-match
  "Unify a bound variable with matching value succeeds"
  (let ((result (unify '?x "alice" '((?x . "alice")))))
    (is-true result)))

(test unify-variable-bound-mismatch
  "Unify a bound variable with non-matching value fails"
  (multiple-value-bind (env success) (unify '?x "bob" '((?x . "alice")))
    (declare (ignore env))
    (is-false success)))
