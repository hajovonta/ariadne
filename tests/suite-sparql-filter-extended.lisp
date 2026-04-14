;;;; tests/suite-sparql-filter-extended.lisp
;;;; Tests for extended SPARQL FILTER support

(in-package #:ariadne/tests)

(in-suite :sparql-filter-extended)

(test filter-uri-equality
  "FILTER with URI equality comparison"
  (let ((g (make-graph :name "filt-uri")))
    (add-triple g "http://ex.org/a" "http://ex.org/type" "http://ex.org/Person")
    (add-triple g "http://ex.org/a" "http://ex.org/name" "Alice")
    (let ((results (sparql-via-algebra g "SELECT ?s ?v WHERE { ?s ?p ?v . FILTER (?p = <http://ex.org/name>) }")))
      (is (= 1 (length results)))
      (is (string= "Alice" (second (first results)))))))

(test filter-not-operator
  "FILTER with ! (NOT) operator"
  (let ((g (make-graph :name "filt-not")))
    (add-triple g "http://ex.org/a" "http://ex.org/val" 1)
    (add-triple g "http://ex.org/b" "http://ex.org/val" 5)
    (let ((results (sparql-via-algebra g "SELECT ?s WHERE { ?s <http://ex.org/val> ?v . FILTER (!(?v = 1)) }")))
      (is (= 1 (length results))))))

(test filter-bound
  "FILTER with bound() function"
  (let ((g (make-graph :name "filt-bound")))
    (add-triple g "http://ex.org/a" "http://ex.org/p" "http://ex.org/b")
    (let ((results (sparql-via-algebra g "SELECT ?s WHERE { ?s <http://ex.org/p> ?o . FILTER (bound(?o)) }")))
      (is (= 1 (length results))))))

(test filter-is-literal
  "FILTER with isLiteral() function"
  (let ((g (make-graph :name "filt-islit")))
    (add-triple g "http://ex.org/a" "http://ex.org/val" (intern-literal "hello" +xsd-string+))
    (add-triple g "http://ex.org/b" "http://ex.org/val" "http://ex.org/uri")
    (let ((results (sparql-via-algebra g "SELECT ?s WHERE { ?s <http://ex.org/val> ?v . FILTER (isLiteral(?v)) }")))
      (is (>= (length results) 1)))))

(test filter-boolean-and
  "FILTER with && operator"
  (let ((g (make-graph :name "filt-and")))
    (add-triple g "http://ex.org/a" "http://ex.org/x" 3)
    (add-triple g "http://ex.org/b" "http://ex.org/x" 7)
    (add-triple g "http://ex.org/c" "http://ex.org/x" 15)
    (let ((results (sparql-via-algebra g "SELECT ?s WHERE { ?s <http://ex.org/x> ?v . FILTER (?v > 2 && ?v < 10) }")))
      (is (= 2 (length results))))))

(test filter-boolean-or
  "FILTER with || operator"
  (let ((g (make-graph :name "filt-or")))
    (add-triple g "http://ex.org/a" "http://ex.org/x" 1)
    (add-triple g "http://ex.org/b" "http://ex.org/x" 5)
    (add-triple g "http://ex.org/c" "http://ex.org/x" 10)
    (let ((results (sparql-via-algebra g "SELECT ?s WHERE { ?s <http://ex.org/x> ?v . FILTER (?v = 1 || ?v = 10) }")))
      (is (= 2 (length results))))))

(test filter-true-false
  "FILTER with boolean constants"
  (let ((g (make-graph :name "filt-bool")))
    (add-triple g "http://ex.org/a" "http://ex.org/p" "v")
    (is (= 1 (length (sparql-via-algebra g "SELECT ?s WHERE { ?s ?p ?o . FILTER (true) }"))))
    (is (= 0 (length (sparql-via-algebra g "SELECT ?s WHERE { ?s ?p ?o . FILTER (false) }"))))))
