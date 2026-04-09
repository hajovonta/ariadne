;;;; examples/family-tree-queries.lisp
;;;; Example queries on the family tree dataset

(ql:quickload :ariadne)
(use-package :ariadne)

;; Load the family tree
(defparameter *g* (make-graph :name "family"))
(import-turtle *g*
  (uiop:read-file-string
    (merge-pathnames "examples/family-tree.ttl"
                     (asdf:system-source-directory :ariadne))))

(format t "Loaded ~A triples~%~%" (triple-count *g*))

;; All people
(format t "=== All People ===~%")
(format-results
  (query *g* '(select (?person ?name ?age)
               (where (?person "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                               "http://example.org/family#Person")
                      (?person "http://www.w3.org/2000/01/rdf-schema#label" ?name)
                      (?person "http://example.org/family#age" ?age))
               (order-by ?age)))
  :vars '(?person ?name ?age)
  :stream *standard-output*)

;; Parent-child pairs
(format t "~%=== Parents and Children ===~%")
(format-results
  (query *g* '(select (?parent ?child)
               (where (?parent "http://example.org/family#hasChild" ?child))))
  :vars '(?parent ?child)
  :stream *standard-output*)

;; Grandchildren of Alice (transitive path)
(format t "~%=== Alice's Descendants (transitive) ===~%")
(format-results
  (query *g* '(select (?descendant)
               (where ("http://example.org/family#alice"
                       (+ "http://example.org/family#hasChild")
                       ?descendant))))
  :vars '(?descendant)
  :stream *standard-output*)

;; People over 40
(format t "~%=== People Over 40 ===~%")
(format-results
  (query *g* '(select (?name ?age)
               (where (?person "http://www.w3.org/2000/01/rdf-schema#label" ?name)
                      (?person "http://example.org/family#age" ?age))
               (filter (> ?age 40))
               (order-by ?age)))
  :vars '(?name ?age)
  :stream *standard-output*)

;; Inference: derive grandparent relationships
(defrule *g* :grandparent
  :when '((?gp "http://example.org/family#hasChild" ?parent)
          (?parent "http://example.org/family#hasChild" ?gc))
  :then '((?gp "http://example.org/family#grandparentOf" ?gc)))
(apply-rules *g*)

(format t "~%=== Grandparents (inferred) ===~%")
(format-results
  (query *g* '(select (?gp ?gc)
               (where (?gp "http://example.org/family#grandparentOf" ?gc))))
  :vars '(?grandparent ?grandchild)
  :stream *standard-output*)
