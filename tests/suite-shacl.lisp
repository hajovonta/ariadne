;;;; tests/suite-shacl.lisp
;;;; Tests for SHACL (Shapes Constraint Language) validation

(in-package #:ariadne/tests)

(in-suite :shacl)

(defvar *sh* "http://www.w3.org/ns/shacl#")
(defvar *rdf* "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(defvar *xsd* "http://www.w3.org/2001/XMLSchema#")
(defvar *ex* "http://example.org/")

(defun sh (name) (concatenate 'string *sh* name))
(defun rdf (name) (concatenate 'string *rdf* name))
(defun xsd (name) (concatenate 'string *xsd* name))
(defun ex (name) (concatenate 'string *ex* name))

;;; ==========================================================================
;;; Target declarations
;;; ==========================================================================

(test shacl-target-class
  "sh:targetClass selects instances of a class"
  (let ((g (make-graph :name "shacl-tc")))
    ;; Shape
    (add-triple g (ex "PersonShape") (rdf "type") (sh "NodeShape"))
    (add-triple g (ex "PersonShape") (sh "targetClass") (ex "Person"))
    (add-triple g (ex "PersonShape") (sh "property") (ex "NameProp"))
    (add-triple g (ex "NameProp") (sh "path") (ex "name"))
    (add-triple g (ex "NameProp") (sh "minCount") "1")
    ;; Data — alice has name, bob doesn't
    (add-triple g (ex "alice") (rdf "type") (ex "Person"))
    (add-triple g (ex "alice") (ex "name") "Alice")
    (add-triple g (ex "bob") (rdf "type") (ex "Person"))
    (let ((report (shacl-validate g)))
      (is-false (getf report :conforms))
      (is (= 1 (length (getf report :results))))
      (is (string= (ex "bob") (getf (first (getf report :results)) :focus-node))))))

(test shacl-target-node
  "sh:targetNode validates specific nodes"
  (let ((g (make-graph :name "shacl-tn")))
    (add-triple g (ex "S1") (rdf "type") (sh "NodeShape"))
    (add-triple g (ex "S1") (sh "targetNode") (ex "alice"))
    (add-triple g (ex "S1") (sh "property") (ex "P1"))
    (add-triple g (ex "P1") (sh "path") (ex "age"))
    (add-triple g (ex "P1") (sh "minCount") "1")
    ;; alice has no age
    (add-triple g (ex "alice") (ex "name") "Alice")
    (let ((report (shacl-validate g)))
      (is-false (getf report :conforms))
      (is (= 1 (length (getf report :results)))))))

(test shacl-target-subjects-of
  "sh:targetSubjectsOf selects subjects of a predicate"
  (let ((g (make-graph :name "shacl-tso")))
    (add-triple g (ex "S1") (rdf "type") (sh "NodeShape"))
    (add-triple g (ex "S1") (sh "targetSubjectsOf") (ex "knows"))
    (add-triple g (ex "S1") (sh "property") (ex "P1"))
    (add-triple g (ex "P1") (sh "path") (ex "name"))
    (add-triple g (ex "P1") (sh "minCount") "1")
    ;; alice knows bob but has no name
    (add-triple g (ex "alice") (ex "knows") (ex "bob"))
    (let ((report (shacl-validate g)))
      (is-false (getf report :conforms)))))

;;; ==========================================================================
;;; Cardinality constraints
;;; ==========================================================================

(test shacl-min-count
  "sh:minCount validates minimum cardinality"
  (let ((g (make-graph :name "shacl-minc")))
    (add-triple g (ex "S1") (rdf "type") (sh "NodeShape"))
    (add-triple g (ex "S1") (sh "targetClass") (ex "Person"))
    (add-triple g (ex "S1") (sh "property") (ex "P1"))
    (add-triple g (ex "P1") (sh "path") (ex "name"))
    (add-triple g (ex "P1") (sh "minCount") "2")
    ;; alice has only 1 name
    (add-triple g (ex "alice") (rdf "type") (ex "Person"))
    (add-triple g (ex "alice") (ex "name") "Alice")
    (let ((report (shacl-validate g)))
      (is-false (getf report :conforms)))))

(test shacl-max-count
  "sh:maxCount validates maximum cardinality"
  (let ((g (make-graph :name "shacl-maxc")))
    (add-triple g (ex "S1") (rdf "type") (sh "NodeShape"))
    (add-triple g (ex "S1") (sh "targetClass") (ex "Person"))
    (add-triple g (ex "S1") (sh "property") (ex "P1"))
    (add-triple g (ex "P1") (sh "path") (ex "name"))
    (add-triple g (ex "P1") (sh "maxCount") "1")
    ;; alice has 2 names
    (add-triple g (ex "alice") (rdf "type") (ex "Person"))
    (add-triple g (ex "alice") (ex "name") "Alice")
    (add-triple g (ex "alice") (ex "name") "Ali")
    (let ((report (shacl-validate g)))
      (is-false (getf report :conforms)))))

(test shacl-cardinality-pass
  "Cardinality constraints pass when satisfied"
  (let ((g (make-graph :name "shacl-card-ok")))
    (add-triple g (ex "S1") (rdf "type") (sh "NodeShape"))
    (add-triple g (ex "S1") (sh "targetClass") (ex "Person"))
    (add-triple g (ex "S1") (sh "property") (ex "P1"))
    (add-triple g (ex "P1") (sh "path") (ex "name"))
    (add-triple g (ex "P1") (sh "minCount") "1")
    (add-triple g (ex "P1") (sh "maxCount") "2")
    (add-triple g (ex "alice") (rdf "type") (ex "Person"))
    (add-triple g (ex "alice") (ex "name") "Alice")
    (let ((report (shacl-validate g)))
      (is-true (getf report :conforms)))))

;;; ==========================================================================
;;; Datatype constraint
;;; ==========================================================================

(test shacl-datatype
  "sh:datatype validates literal datatype"
  (let ((g (make-graph :name "shacl-dt")))
    (add-triple g (ex "S1") (rdf "type") (sh "NodeShape"))
    (add-triple g (ex "S1") (sh "targetClass") (ex "Person"))
    (add-triple g (ex "S1") (sh "property") (ex "P1"))
    (add-triple g (ex "P1") (sh "path") (ex "age"))
    (add-triple g (ex "P1") (sh "datatype") (xsd "integer"))
    ;; alice has string age
    (add-triple g (ex "alice") (rdf "type") (ex "Person"))
    (add-triple g (ex "alice") (ex "age") "thirty")
    (let ((report (shacl-validate g)))
      (is-false (getf report :conforms)))))

(test shacl-datatype-integer-pass
  "sh:datatype xsd:integer passes for numeric strings"
  (let ((g (make-graph :name "shacl-dt-ok")))
    (add-triple g (ex "S1") (rdf "type") (sh "NodeShape"))
    (add-triple g (ex "S1") (sh "targetClass") (ex "Person"))
    (add-triple g (ex "S1") (sh "property") (ex "P1"))
    (add-triple g (ex "P1") (sh "path") (ex "age"))
    (add-triple g (ex "P1") (sh "datatype") (xsd "integer"))
    (add-triple g (ex "alice") (rdf "type") (ex "Person"))
    (add-triple g (ex "alice") (ex "age") 30)
    (let ((report (shacl-validate g)))
      (is-true (getf report :conforms)))))

;;; ==========================================================================
;;; Pattern constraint
;;; ==========================================================================

(test shacl-pattern
  "sh:pattern validates regex on values"
  (let ((g (make-graph :name "shacl-pat")))
    (add-triple g (ex "S1") (rdf "type") (sh "NodeShape"))
    (add-triple g (ex "S1") (sh "targetClass") (ex "Person"))
    (add-triple g (ex "S1") (sh "property") (ex "P1"))
    (add-triple g (ex "P1") (sh "path") (ex "email"))
    (add-triple g (ex "P1") (sh "pattern") "^.+@.+\\..+$")
    (add-triple g (ex "alice") (rdf "type") (ex "Person"))
    (add-triple g (ex "alice") (ex "email") "not-an-email")
    (let ((report (shacl-validate g)))
      (is-false (getf report :conforms)))))

;;; ==========================================================================
;;; nodeKind constraint
;;; ==========================================================================

(test shacl-node-kind-iri
  "sh:nodeKind sh:IRI rejects literals"
  (let ((g (make-graph :name "shacl-nk")))
    (add-triple g (ex "S1") (rdf "type") (sh "NodeShape"))
    (add-triple g (ex "S1") (sh "targetClass") (ex "Person"))
    (add-triple g (ex "S1") (sh "property") (ex "P1"))
    (add-triple g (ex "P1") (sh "path") (ex "knows"))
    (add-triple g (ex "P1") (sh "nodeKind") (sh "IRI"))
    (add-triple g (ex "alice") (rdf "type") (ex "Person"))
    (add-triple g (ex "alice") (ex "knows") "just-a-string")
    (let ((report (shacl-validate g)))
      (is-false (getf report :conforms)))))

;;; ==========================================================================
;;; sh:in constraint
;;; ==========================================================================

(test shacl-in
  "sh:in validates value is in allowed list"
  (let ((g (make-graph :name "shacl-in")))
    (add-triple g (ex "S1") (rdf "type") (sh "NodeShape"))
    (add-triple g (ex "S1") (sh "targetClass") (ex "Person"))
    (add-triple g (ex "S1") (sh "property") (ex "P1"))
    (add-triple g (ex "P1") (sh "path") (ex "status"))
    ;; Allowed values as an RDF list (simplified: use multiple sh:in triples)
    (add-triple g (ex "P1") (sh "in") "active")
    (add-triple g (ex "P1") (sh "in") "inactive")
    (add-triple g (ex "alice") (rdf "type") (ex "Person"))
    (add-triple g (ex "alice") (ex "status") "unknown")
    (let ((report (shacl-validate g)))
      (is-false (getf report :conforms)))))

;;; ==========================================================================
;;; Validation report format
;;; ==========================================================================

(test shacl-report-structure
  "Validation report has standard structure"
  (let ((g (make-graph :name "shacl-report")))
    (add-triple g (ex "S1") (rdf "type") (sh "NodeShape"))
    (add-triple g (ex "S1") (sh "targetClass") (ex "Person"))
    (add-triple g (ex "S1") (sh "property") (ex "P1"))
    (add-triple g (ex "P1") (sh "path") (ex "name"))
    (add-triple g (ex "P1") (sh "minCount") "1")
    (add-triple g (ex "alice") (rdf "type") (ex "Person"))
    (let ((report (shacl-validate g)))
      (is (member :conforms report))
      (is (member :results report))
      (let ((r (first (getf report :results))))
        (is (getf r :focus-node))
        (is (getf r :result-path))
        (is (getf r :source-shape))
        (is (getf r :result-message))))))

(test shacl-conforms-empty
  "Graph with no shapes conforms"
  (let ((g (make-graph :name "shacl-empty")))
    (add-triple g (ex "alice") (ex "name") "Alice")
    (let ((report (shacl-validate g)))
      (is-true (getf report :conforms)))))
