;;;; tests/suite-owl-reasoning.lisp
;;;; OWL/RDFS entailment rules

(in-package #:ariadne/tests)
(in-suite :owl-reasoning)

(defparameter *rdf-type* "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
(defparameter *rdfs-subclass* "http://www.w3.org/2000/01/rdf-schema#subClassOf")
(defparameter *rdfs-subprop* "http://www.w3.org/2000/01/rdf-schema#subPropertyOf")
(defparameter *rdfs-domain* "http://www.w3.org/2000/01/rdf-schema#domain")
(defparameter *rdfs-range* "http://www.w3.org/2000/01/rdf-schema#range")
(defparameter *owl-inverse* "http://www.w3.org/2002/07/owl#inverseOf")
(defparameter *owl-transitive* "http://www.w3.org/2002/07/owl#TransitiveProperty")
(defparameter *owl-symmetric* "http://www.w3.org/2002/07/owl#SymmetricProperty")
(defparameter *owl-sameas* "http://www.w3.org/2002/07/owl#sameAs")

;; =============================================================================
;; RDFS subClassOf
;; =============================================================================

(test rdfs-subclass-direct
  "Type inferred through direct subclass"
  (let ((g (make-graph)))
    (add-triple g "Rex" *rdf-type* "Dog")
    (add-triple g "Dog" *rdfs-subclass* "Animal")
    (apply-owl-rules g)
    (is-true (has-triple-p g "Rex" *rdf-type* "Animal"))))

(test rdfs-subclass-transitive
  "Type inferred through transitive subclass chain"
  (let ((g (make-graph)))
    (add-triple g "Rex" *rdf-type* "Dog")
    (add-triple g "Dog" *rdfs-subclass* "Animal")
    (add-triple g "Animal" *rdfs-subclass* "LivingThing")
    (apply-owl-rules g)
    (is-true (has-triple-p g "Rex" *rdf-type* "LivingThing"))))

;; =============================================================================
;; RDFS subPropertyOf
;; =============================================================================

(test rdfs-subproperty
  "Triple inferred through subPropertyOf"
  (let ((g (make-graph)))
    (add-triple g "alice" "hasMother" "carol")
    (add-triple g "hasMother" *rdfs-subprop* "hasParent")
    (apply-owl-rules g)
    (is-true (has-triple-p g "alice" "hasParent" "carol"))))

;; =============================================================================
;; RDFS domain / range
;; =============================================================================

(test rdfs-domain
  "Type inferred from property domain"
  (let ((g (make-graph)))
    (add-triple g "alice" "writes" "paper1")
    (add-triple g "writes" *rdfs-domain* "Author")
    (apply-owl-rules g)
    (is-true (has-triple-p g "alice" *rdf-type* "Author"))))

(test rdfs-range
  "Type inferred from property range"
  (let ((g (make-graph)))
    (add-triple g "alice" "writes" "paper1")
    (add-triple g "writes" *rdfs-range* "Publication")
    (apply-owl-rules g)
    (is-true (has-triple-p g "paper1" *rdf-type* "Publication"))))

;; =============================================================================
;; OWL inverseOf
;; =============================================================================

(test owl-inverse-of
  "Inverse triple inferred"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "knows" *owl-inverse* "knownBy")
    (apply-owl-rules g)
    (is-true (has-triple-p g "bob" "knownBy" "alice"))))

;; =============================================================================
;; OWL TransitiveProperty
;; =============================================================================

(test owl-transitive-property
  "Transitive closure inferred"
  (let ((g (make-graph)))
    (add-triple g "ancestor" *rdf-type* *owl-transitive*)
    (add-triple g "alice" "ancestor" "bob")
    (add-triple g "bob" "ancestor" "charlie")
    (apply-owl-rules g)
    (is-true (has-triple-p g "alice" "ancestor" "charlie"))))

;; =============================================================================
;; OWL SymmetricProperty
;; =============================================================================

(test owl-symmetric-property
  "Symmetric triple inferred"
  (let ((g (make-graph)))
    (add-triple g "friendOf" *rdf-type* *owl-symmetric*)
    (add-triple g "alice" "friendOf" "bob")
    (apply-owl-rules g)
    (is-true (has-triple-p g "bob" "friendOf" "alice"))))

;; =============================================================================
;; OWL sameAs
;; =============================================================================

(test owl-sameas
  "sameAs copies triples to equivalent entity"
  (let ((g (make-graph)))
    (add-triple g "alice" "age" 30)
    (add-triple g "alice" *owl-sameas* "alice_v2")
    (apply-owl-rules g)
    (is-true (has-triple-p g "alice_v2" "age" 30))))

;; =============================================================================
;; Combined
;; =============================================================================

(test owl-combined
  "Multiple OWL rules interact correctly"
  (let ((g (make-graph)))
    (add-triple g "Rex" *rdf-type* "Dog")
    (add-triple g "Dog" *rdfs-subclass* "Animal")
    (add-triple g "alice" "owns" "Rex")
    (add-triple g "owns" *rdfs-domain* "Person")
    (add-triple g "owns" *rdfs-range* "Animal")
    (apply-owl-rules g)
    (is-true (has-triple-p g "Rex" *rdf-type* "Animal"))
    (is-true (has-triple-p g "alice" *rdf-type* "Person"))
    ;; Rex already typed Animal via subclass, range also says Animal — no conflict
    (is-true (has-triple-p g "Rex" *rdf-type* "Animal"))))
