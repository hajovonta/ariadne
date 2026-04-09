;;;; tests/suite-prefix-registry.lisp
;;;; Prefix registry for short URIs in queries

(in-package #:ariadne/tests)
(in-suite :prefix-registry)

(test register-and-expand
  "Register prefix and expand in queries"
  (let ((g (make-graph)))
    (register-prefix g "foaf" "http://xmlns.com/foaf/0.1/")
    (add-triple g "http://xmlns.com/foaf/0.1/alice" "http://xmlns.com/foaf/0.1/name" "Alice")
    (let ((result (query g '(select (?name) (where ("foaf:alice" "foaf:name" ?name))))))
      (is (= 1 (length result)))
      (is (equal "Alice" (caar result))))))

(test register-multiple-prefixes
  "Multiple prefixes work together"
  (let ((g (make-graph)))
    (register-prefix g "ex" "http://example.org/")
    (register-prefix g "foaf" "http://xmlns.com/foaf/0.1/")
    (add-triple g "http://example.org/alice" "http://xmlns.com/foaf/0.1/knows" "http://example.org/bob")
    (let ((result (query g '(select (?who) (where ("ex:alice" "foaf:knows" ?who))))))
      (is (= 1 (length result))))))

(test prefix-in-add-triple
  "Prefixes expand in add-triple"
  (let ((g (make-graph)))
    (register-prefix g "ex" "http://example.org/")
    (add-triple g "ex:alice" "ex:knows" "ex:bob")
    (is-true (has-triple-p g "http://example.org/alice"
                             "http://example.org/knows"
                             "http://example.org/bob"))))

(test list-prefixes
  "List registered prefixes"
  (let ((g (make-graph)))
    (register-prefix g "ex" "http://example.org/")
    (register-prefix g "foaf" "http://xmlns.com/foaf/0.1/")
    (is (= 2 (length (graph-prefixes g))))))

(test common-prefixes
  "Register common well-known prefixes"
  (let ((g (make-graph)))
    (register-common-prefixes g)
    (is (not (null (graph-prefixes g))))
    (add-triple g "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "rdf:type" "test")
    ;; rdf: should be registered
    (is-true (has-triple-p g "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" 
                             "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "test"))))
