;;;; tests/suite-skolemization.lisp
;;;; Tests for blank node skolemization

(in-package #:ariadne/tests)

(in-suite :skolemization)

(test skolemize-blank-nodes
  "Blank nodes get replaced with stable URIs"
  (let ((g (make-graph :name "skolem-test")))
    (add-triple g "_:b1" "http://ex.org/p" "http://ex.org/o")
    (add-triple g "http://ex.org/s" "http://ex.org/p" "_:b2")
    (skolemize-blank-nodes g)
    (let ((triples (get-triples g)))
      (is (= 2 (length triples)))
      ;; No blank nodes remain
      (dolist (tr triples)
        (is-false (and (stringp (triple-subject tr))
                       (eql 0 (search "_:" (triple-subject tr)))))
        (is-false (and (stringp (triple-object tr))
                       (eql 0 (search "_:" (triple-object tr)))))))))

(test skolemize-uses-well-known-prefix
  "Skolemized URIs use /.well-known/genid/ prefix"
  (let ((g (make-graph :name "skolem-prefix")))
    (add-triple g "_:x" "http://ex.org/p" "http://ex.org/o")
    (skolemize-blank-nodes g)
    (let ((s (triple-subject (first (get-triples g)))))
      (is (search "/.well-known/genid/" s)))))

(test skolemize-preserves-non-blank
  "Non-blank-node triples are unchanged"
  (let ((g (make-graph :name "skolem-preserve")))
    (add-triple g "http://ex.org/s" "http://ex.org/p" "http://ex.org/o")
    (skolemize-blank-nodes g)
    (is (has-triple-p g "http://ex.org/s" "http://ex.org/p" "http://ex.org/o"))))

(test skolemize-consistent-mapping
  "Same blank node label maps to same URI"
  (let ((g (make-graph :name "skolem-consistent")))
    (add-triple g "_:b1" "http://ex.org/p1" "http://ex.org/o1")
    (add-triple g "_:b1" "http://ex.org/p2" "http://ex.org/o2")
    (skolemize-blank-nodes g)
    (let* ((triples (get-triples g))
           (subjects (mapcar #'triple-subject triples)))
      ;; Both triples should have the same skolemized subject
      (is (string= (first subjects) (second subjects))))))
