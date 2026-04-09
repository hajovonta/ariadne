;;;; rdf-xml.lisp
;;;; RDF/XML import (regex-based, no XML library dependency)

(in-package #:ariadne)

(defun import-rdf-xml (g data)
  "Import RDF/XML format string into graph G."
  (let ((namespaces (make-hash-table :test 'equal))
        (rdf-type "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
    ;; Extract namespace declarations
    (cl-ppcre:do-matches-as-strings (m "xmlns:(\\w+)=\"([^\"]+)\"" data)
      (cl-ppcre:register-groups-bind (prefix uri) ("xmlns:(\\w+)=\"([^\"]+)\"" m)
        (setf (gethash prefix namespaces) uri)))
    ;; Process rdf:Description and typed elements
    (cl-ppcre:do-matches-as-strings
        (m "<(\\w+:\\w+|rdf:Description)[^>]*rdf:about=\"([^\"]+)\"[^>]*/?>([\\s\\S]*?)</\\1>|<(\\w+:\\w+|rdf:Description)[^>]*rdf:about=\"([^\"]+)\"[^>]*/>" data)
      (let ((subject nil) (tag nil) (body nil))
        ;; Try self-closing first
        (cl-ppcre:register-groups-bind (t1 s1 b1 t2 s2)
            ("<(\\w+:\\w+|rdf:Description)[^>]*rdf:about=\"([^\"]+)\"[^>]*/?>([\\s\\S]*?)</\\1>|<(\\w+:\\w+|rdf:Description)[^>]*rdf:about=\"([^\"]+)\"[^>]*/>" m)
          (cond
            (s1 (setf subject s1 tag t1 body b1))
            (s2 (setf subject s2 tag t2 body ""))))
        (when subject
          ;; If tag is not rdf:Description, it's a typed node
          (when (and tag (not (string= tag "rdf:Description")))
            (let ((type-uri (expand-rdf-xml-name tag namespaces)))
              (when type-uri
                (add-triple g subject rdf-type type-uri))))
          ;; Parse properties in body
          (when body
            (parse-rdf-xml-properties g subject body namespaces)))))))

(defun parse-rdf-xml-properties (g subject body namespaces)
  "Parse property elements within an rdf:Description body."
  ;; Resource references: <ex:knows rdf:resource="..."/>
  (cl-ppcre:do-matches-as-strings
      (m "<(\\w+:\\w+)\\s+rdf:resource=\"([^\"]+)\"\\s*/>" body)
    (cl-ppcre:register-groups-bind (pred obj)
        ("<(\\w+:\\w+)\\s+rdf:resource=\"([^\"]+)\"\\s*/>" m)
      (let ((pred-uri (expand-rdf-xml-name pred namespaces)))
        (when pred-uri
          (add-triple g subject pred-uri obj)))))
  ;; Literal values: <ex:name>Alice</ex:name>
  (cl-ppcre:do-matches-as-strings
      (m "<(\\w+:\\w+)>([^<]+)</\\1>" body)
    (cl-ppcre:register-groups-bind (pred val)
        ("<(\\w+:\\w+)>([^<]+)</\\1>" m)
      (let ((pred-uri (expand-rdf-xml-name pred namespaces)))
        (when pred-uri
          (add-triple g subject pred-uri val))))))

(defun expand-rdf-xml-name (name namespaces)
  "Expand a prefixed name like 'ex:knows' to full URI."
  (let ((colon (position #\: name)))
    (when colon
      (let* ((prefix (subseq name 0 colon))
             (local (subseq name (1+ colon)))
             (base (gethash prefix namespaces)))
        (when base
          (concatenate 'string base local))))))
