;;;; tests/suite-inference.lisp
;;;; Inference rules engine

(in-package #:ariadne/tests)
(in-suite :inference)

;; =============================================================================
;; Basic Rules
;; =============================================================================

(test rule-simple-materialization
  "A rule materializes new triples"
  (let ((g (make-graph)))
    (add-triple g "alice" "parent" "bob")
    (add-triple g "bob" "parent" "charlie")
    (defrule g :ancestor
      :when '((?a "parent" ?b))
      :then '((?a "ancestor" ?b)))
    (apply-rules g)
    (is-true (has-triple-p g "alice" "ancestor" "bob"))
    (is-true (has-triple-p g "bob" "ancestor" "charlie"))))

(test rule-transitive
  "Rules can chain transitively"
  (let ((g (make-graph)))
    (add-triple g "alice" "parent" "bob")
    (add-triple g "bob" "parent" "charlie")
    (defrule g :ancestor
      :when '((?a "parent" ?b))
      :then '((?a "ancestor" ?b)))
    (defrule g :ancestor-transitive
      :when '((?a "ancestor" ?b) (?b "ancestor" ?c))
      :then '((?a "ancestor" ?c)))
    (apply-rules g)
    (is-true (has-triple-p g "alice" "ancestor" "charlie"))))

(test rule-subclass
  "RDFS-style subclass inference"
  (let ((g (make-graph)))
    (add-triple g "dog" "subClassOf" "animal")
    (add-triple g "animal" "subClassOf" "living-thing")
    (add-triple g "rex" "type" "dog")
    (defrule g :subclass-type
      :when '((?x "type" ?class) (?class "subClassOf" ?super))
      :then '((?x "type" ?super)))
    (apply-rules g)
    (is-true (has-triple-p g "rex" "type" "animal"))
    (is-true (has-triple-p g "rex" "type" "living-thing"))))

(test rule-symmetric
  "Symmetric property inference"
  (let ((g (make-graph)))
    (add-triple g "alice" "friendOf" "bob")
    (defrule g :symmetric-friend
      :when '((?a "friendOf" ?b))
      :then '((?b "friendOf" ?a)))
    (apply-rules g)
    (is-true (has-triple-p g "bob" "friendOf" "alice"))))

(test rule-no-duplicates
  "Rules don't create duplicate triples"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (defrule g :identity
      :when '((?a "knows" ?b))
      :then '((?a "knows" ?b)))
    (let ((before (triple-count g)))
      (apply-rules g)
      (is (= before (triple-count g))))))

(test rule-fixed-point
  "apply-rules reaches a fixed point"
  (let ((g (make-graph)))
    (add-triple g "a" "link" "b")
    (add-triple g "b" "link" "c")
    (add-triple g "c" "link" "d")
    (defrule g :transitive-link
      :when '((?a "link" ?b) (?b "link" ?c))
      :then '((?a "link" ?c)))
    (apply-rules g)
    ;; a->b, b->c, c->d, a->c, b->d, a->d = 6
    (is (= 6 (triple-count g)))))

;; =============================================================================
;; Rule Management
;; =============================================================================

(test list-rules
  "List all defined rules"
  (let ((g (make-graph)))
    (defrule g :rule-1
      :when '((?a "knows" ?b))
      :then '((?a "connected" ?b)))
    (defrule g :rule-2
      :when '((?a "likes" ?b))
      :then '((?a "connected" ?b)))
    (is (= 2 (length (graph-rules g))))))

(test remove-rule
  "Remove a rule by name"
  (let ((g (make-graph)))
    (defrule g :my-rule
      :when '((?a "knows" ?b))
      :then '((?a "connected" ?b)))
    (remove-rule g :my-rule)
    (is (= 0 (length (graph-rules g))))))
