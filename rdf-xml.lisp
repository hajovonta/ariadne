;;;; rdf-xml.lisp
;;;; RDF/XML import using CXML

(in-package #:ariadne)

(defvar *blank-counter* 0)

(defun rdf-attr (element local-name)
  "Get attribute value by local-name in the RDF namespace."
  (dolist (a (stp:list-attributes element))
    (when (string= (stp:local-name a) local-name)
      (return (stp:value a)))))

(defun child-text (element)
  "Get concatenated text content of an element."
  (let ((parts nil))
    (stp:do-children (c element)
      (when (typep c 'stp:text)
        (push (stp:data c) parts)))
    (when parts (apply #'concatenate 'string (nreverse parts)))))

(defun elem-uri (element)
  "Get full URI for an element (namespace + local-name)."
  (concatenate 'string (stp:namespace-uri element) (stp:local-name element)))

(defun parse-typed-value (text datatype)
  "Parse a typed literal value string."
  (cond
    ((search "integer" datatype) (parse-integer text :junk-allowed t))
    ((search "decimal" datatype) (let ((*read-eval* nil)) (read-from-string text)))
    ((search "double" datatype) (let ((*read-eval* nil)) (read-from-string text)))
    ((search "float" datatype) (let ((*read-eval* nil)) (read-from-string text)))
    ((search "boolean" datatype) (string-equal text "true"))
    (t text)))

(defun import-rdf-xml (g data &key base-uri)
  "Import RDF/XML format string into graph G."
  (let* ((rdf-type "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
         (doc (cxml:parse data (stp:make-builder)))
         (root (stp:document-element doc)))
    (stp:do-children (desc root)
      (when (typep desc 'stp:element)
        (import-rdf-xml-description g desc rdf-type base-uri)))))

(defun import-rdf-xml-description (g desc rdf-type &optional base-uri)
  "Import one rdf:Description (or typed node) element."
  (let* ((about (rdf-attr desc "about"))
         (node-id (rdf-attr desc "nodeID"))
         (subject (or (and about (if (and base-uri (zerop (length about))) base-uri about))
                      (when node-id (concatenate 'string "_:" node-id))
                      (format nil "_:rdfxml~A" (incf *blank-counter*)))))
    ;; Typed node
    (unless (string= (stp:local-name desc) "Description")
      (add-triple g subject rdf-type (elem-uri desc)))
    ;; Properties
    (stp:do-children (prop desc)
      (when (typep prop 'stp:element)
        (let ((pred (elem-uri prop))
              (resource (rdf-attr prop "resource"))
              (datatype (rdf-attr prop "datatype"))
              (lang (rdf-attr prop "lang"))
              (prop-nid (rdf-attr prop "nodeID"))
              (parse-type (rdf-attr prop "parseType")))
          (cond
            (resource (add-triple g subject pred
                                  (if (and base-uri (zerop (length resource))) base-uri resource)))
            (prop-nid (add-triple g subject pred (concatenate 'string "_:" prop-nid)))
            ((and parse-type (string= parse-type "Resource"))
             (let ((bnode (format nil "_:rdfxml~A" (incf *blank-counter*))))
               (add-triple g subject pred bnode)
               (stp:do-children (inner prop)
                 (when (typep inner 'stp:element)
                   (let ((text (child-text inner)))
                     (when text (add-triple g bnode (elem-uri inner) text)))))))
            ;; Nested element children → object is a resource
            ((some (lambda (c) (typep c 'stp:element)) (stp:list-children prop))
             (stp:do-children (child prop)
               (when (typep child 'stp:element)
                 (let ((obj-about (rdf-attr child "about"))
                       (obj-nid (rdf-attr child "nodeID"))
                       (bnode (format nil "_:rdfxml~A" (incf *blank-counter*))))
                   (let ((obj (or obj-about
                                  (when obj-nid (concatenate 'string "_:" obj-nid))
                                  bnode)))
                     (add-triple g subject pred obj)
                     (import-rdf-xml-description g child rdf-type))))))
            ;; Literal
            (t (let ((text (child-text prop)))
                 (when text
                   (add-triple g subject pred
                               (cond
                                 (datatype (intern-literal (parse-typed-value text datatype) datatype))
                                 (lang (intern-literal text +rdf-langstring+ lang))
                                 (t text))))))))))))
