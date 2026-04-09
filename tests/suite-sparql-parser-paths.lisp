;;;; tests/suite-sparql-parser-paths.lisp
;;;; SPARQL parser: property paths and BIND

(in-package #:ariadne/tests)
(in-suite :sparql-parser-paths)

(test sparql-parse-transitive-path
  "Parse transitive property path +"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (let ((result (sparql g "SELECT ?who WHERE { \"alice\" \"knows\"+ ?who }")))
      (is (= 2 (length result))))))

(test sparql-parse-kleene-path
  "Parse Kleene star property path *"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "bob" "knows" "charlie")
    (let ((result (sparql g "SELECT ?who WHERE { \"alice\" \"knows\"* ?who }")))
      (is (= 3 (length result))))))  ; alice, bob, charlie

(test sparql-parse-inverse-path
  "Parse inverse property path ^"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (let ((result (sparql g "SELECT ?who WHERE { \"bob\" ^\"knows\" ?who }")))
      (is (= 1 (length result)))
      (is (equal "alice" (caar result))))))

(test sparql-parse-bind
  "Parse BIND clause"
  (let ((g (make-graph)))
    (add-triple g "alice" "age" 30)
    (let ((result (sparql g "SELECT ?name ?age WHERE { ?name \"age\" ?age . BIND (?doubled AS ?age * 2) }")))
      ;; Should have results with the bound variable
      (is (>= (length result) 1)))))
