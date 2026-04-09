;;;; tests/suite-rdf-xml.lisp
;;;; RDF/XML import

(in-package #:ariadne/tests)
(in-suite :rdf-xml)

(test rdf-xml-basic
  "Import basic RDF/XML"
  (let ((g (make-graph)))
    (import-rdf-xml g
      "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
                xmlns:ex=\"http://example.org/\">
         <rdf:Description rdf:about=\"http://example.org/alice\">
           <ex:knows rdf:resource=\"http://example.org/bob\"/>
         </rdf:Description>
       </rdf:RDF>")
    (is (= 1 (triple-count g)))
    (is-true (has-triple-p g "http://example.org/alice"
                             "http://example.org/knows"
                             "http://example.org/bob"))))

(test rdf-xml-literal
  "Import RDF/XML with literal values"
  (let ((g (make-graph)))
    (import-rdf-xml g
      "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
                xmlns:ex=\"http://example.org/\">
         <rdf:Description rdf:about=\"http://example.org/alice\">
           <ex:name>Alice</ex:name>
         </rdf:Description>
       </rdf:RDF>")
    (is (= 1 (triple-count g)))
    (is-true (has-triple-p g "http://example.org/alice"
                             "http://example.org/name"
                             "Alice"))))

(test rdf-xml-type
  "Import RDF/XML with rdf:type"
  (let ((g (make-graph)))
    (import-rdf-xml g
      "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
                xmlns:ex=\"http://example.org/\">
         <ex:Person rdf:about=\"http://example.org/alice\">
           <ex:name>Alice</ex:name>
         </ex:Person>
       </rdf:RDF>")
    (is (= 2 (triple-count g)))
    (is-true (has-triple-p g "http://example.org/alice"
                             "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                             "http://example.org/Person"))))

(test rdf-xml-multiple
  "Import RDF/XML with multiple subjects"
  (let ((g (make-graph)))
    (import-rdf-xml g
      "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
                xmlns:ex=\"http://example.org/\">
         <rdf:Description rdf:about=\"http://example.org/alice\">
           <ex:knows rdf:resource=\"http://example.org/bob\"/>
           <ex:name>Alice</ex:name>
         </rdf:Description>
         <rdf:Description rdf:about=\"http://example.org/bob\">
           <ex:name>Bob</ex:name>
         </rdf:Description>
       </rdf:RDF>")
    (is (= 3 (triple-count g)))))
