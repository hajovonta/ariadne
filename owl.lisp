;;;; owl.lisp
;;;; OWL/RDFS entailment rules

(in-package #:ariadne)

(defparameter *rdf-type* "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
(defparameter *rdfs-subclass* "http://www.w3.org/2000/01/rdf-schema#subClassOf")
(defparameter *rdfs-subprop* "http://www.w3.org/2000/01/rdf-schema#subPropertyOf")
(defparameter *rdfs-domain* "http://www.w3.org/2000/01/rdf-schema#domain")
(defparameter *rdfs-range* "http://www.w3.org/2000/01/rdf-schema#range")
(defparameter *owl-inverse* "http://www.w3.org/2002/07/owl#inverseOf")
(defparameter *owl-transitive* "http://www.w3.org/2002/07/owl#TransitiveProperty")
(defparameter *owl-symmetric* "http://www.w3.org/2002/07/owl#SymmetricProperty")
(defparameter *owl-sameas* "http://www.w3.org/2002/07/owl#sameAs")

(defun apply-owl-rules (g)
  "Apply RDFS/OWL entailment rules until fixed point."
  (loop for new-count = (owl-step g)
        while (> new-count 0)))

(defun owl-step (g)
  "One pass of all OWL/RDFS rules. Returns count of new triples added."
  (let ((before (triple-count g)))
    ;; rdfs:subClassOf — transitive + type propagation
    (dolist (sc (get-triples g :predicate *rdfs-subclass*))
      (let ((sub (triple-subject sc))
            (super (triple-object sc)))
        ;; Propagate types
        (dolist (inst (get-triples g :predicate *rdf-type*))
          (when (equal (triple-object inst) sub)
            (add-triple g (triple-subject inst) *rdf-type* super)))
        ;; Transitive subclass
        (dolist (sc2 (get-triples g :predicate *rdfs-subclass*))
          (when (equal (triple-subject sc2) super)
            (add-triple g sub *rdfs-subclass* (triple-object sc2))))))
    ;; rdfs:subPropertyOf
    (dolist (sp (get-triples g :predicate *rdfs-subprop*))
      (let ((sub-prop (triple-subject sp))
            (super-prop (triple-object sp)))
        (dolist (tr (get-triples g :predicate sub-prop))
          (add-triple g (triple-subject tr) super-prop (triple-object tr)))))
    ;; rdfs:domain
    (dolist (d (get-triples g :predicate *rdfs-domain*))
      (let ((prop (triple-subject d))
            (class (triple-object d)))
        (dolist (tr (get-triples g :predicate prop))
          (add-triple g (triple-subject tr) *rdf-type* class))))
    ;; rdfs:range
    (dolist (r (get-triples g :predicate *rdfs-range*))
      (let ((prop (triple-subject r))
            (class (triple-object r)))
        (dolist (tr (get-triples g :predicate prop))
          (add-triple g (triple-object tr) *rdf-type* class))))
    ;; owl:inverseOf
    (dolist (inv (get-triples g :predicate *owl-inverse*))
      (let ((p1 (triple-subject inv))
            (p2 (triple-object inv)))
        (dolist (tr (get-triples g :predicate p1))
          (add-triple g (triple-object tr) p2 (triple-subject tr)))
        (dolist (tr (get-triples g :predicate p2))
          (add-triple g (triple-object tr) p1 (triple-subject tr)))))
    ;; owl:TransitiveProperty
    (dolist (tp (get-triples g :predicate *rdf-type*))
      (when (equal (triple-object tp) *owl-transitive*)
        (let ((prop (triple-subject tp)))
          (dolist (t1 (get-triples g :predicate prop))
            (dolist (t2 (get-triples g :predicate prop))
              (when (equal (triple-object t1) (triple-subject t2))
                (add-triple g (triple-subject t1) prop (triple-object t2))))))))
    ;; owl:SymmetricProperty
    (dolist (sp (get-triples g :predicate *rdf-type*))
      (when (equal (triple-object sp) *owl-symmetric*)
        (let ((prop (triple-subject sp)))
          (dolist (tr (get-triples g :predicate prop))
            (add-triple g (triple-object tr) prop (triple-subject tr))))))
    ;; owl:sameAs
    (dolist (sa (get-triples g :predicate *owl-sameas*))
      (let ((a (triple-subject sa))
            (b (triple-object sa)))
        (dolist (tr (get-triples g :subject a))
          (unless (equal (triple-predicate tr) *owl-sameas*)
            (add-triple g b (triple-predicate tr) (triple-object tr))))
        (dolist (tr (get-triples g :subject b))
          (unless (equal (triple-predicate tr) *owl-sameas*)
            (add-triple g a (triple-predicate tr) (triple-object tr))))))
    (- (triple-count g) before)))
