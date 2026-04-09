;;;; tests/suite-schema-validation.lisp
;;;; Schema validation and constraint checking

(in-package #:ariadne/tests)
(in-suite :schema-validation)

(defparameter *type* "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")

(test define-schema
  "Define a schema with expected predicates and types"
  (let ((g (make-graph)))
    (define-schema g
      :classes '(("Person" :properties (("name" :type string :required t)
                                        ("age" :type number)))))
    (is (not (null (graph-schema g))))))

(test validate-valid-graph
  "Valid graph passes validation"
  (let ((g (make-graph)))
    (define-schema g
      :classes '(("Person" :properties (("name" :type string :required t)
                                        ("age" :type number)))))
    (add-triple g "alice" *type* "Person")
    (add-triple g "alice" "name" "Alice")
    (add-triple g "alice" "age" 30)
    (is (null (validate-graph g)))))

(test validate-missing-required
  "Missing required property detected"
  (let ((g (make-graph)))
    (define-schema g
      :classes '(("Person" :properties (("name" :type string :required t)))))
    (add-triple g "alice" *type* "Person")
    (let ((errors (validate-graph g)))
      (is (= 1 (length errors)))
      (is (search "required" (first errors))))))

(test validate-wrong-type
  "Wrong property type detected"
  (let ((g (make-graph)))
    (define-schema g
      :classes '(("Person" :properties (("age" :type number)))))
    (add-triple g "alice" *type* "Person")
    (add-triple g "alice" "age" "thirty")
    (let ((errors (validate-graph g)))
      (is (= 1 (length errors)))
      (is (search "type" (first errors))))))

(test validate-max-cardinality
  "Max cardinality violation detected"
  (let ((g (make-graph)))
    (define-schema g
      :classes '(("Person" :properties (("spouse" :max 1)))))
    (add-triple g "alice" *type* "Person")
    (add-triple g "alice" "spouse" "bob")
    (add-triple g "alice" "spouse" "charlie")
    (let ((errors (validate-graph g)))
      (is (= 1 (length errors)))
      (is (search "cardinality" (first errors))))))

(test validate-min-cardinality
  "Min cardinality same as required"
  (let ((g (make-graph)))
    (define-schema g
      :classes '(("Person" :properties (("name" :min 1)))))
    (add-triple g "alice" *type* "Person")
    (let ((errors (validate-graph g)))
      (is (= 1 (length errors))))))

(test find-similar-entities
  "Find entities with similar labels"
  (let ((g (make-graph)))
    (add-triple g "alice1" "name" "Alice Smith")
    (add-triple g "alice2" "name" "Alice Smth")
    (add-triple g "bob" "name" "Bob Jones")
    (let ((dupes (find-similar-entities g :predicate "name" :threshold 0.7)))
      (is (= 1 (length dupes))))))
