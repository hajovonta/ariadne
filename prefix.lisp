;;;; prefix.lisp
;;;; Prefix registry for short URIs

(in-package #:ariadne)

(defun graph-prefixes (g)
  (getf (graph-extra g) :prefixes))

(defun (setf graph-prefixes) (val g)
  (setf (getf (graph-extra g) :prefixes) val))

(defun register-prefix (g short-name uri)
  "Register a prefix so 'short:local' expands to 'uri+local'."
  (let ((key (concatenate 'string short-name ":")))
    (setf (graph-prefixes g)
          (acons key uri (remove key (graph-prefixes g) :key #'car :test #'equal)))))

(defun expand-prefix (g term)
  "Expand a prefixed term using the graph's prefix registry. Returns expanded or original."
  (when (stringp term)
    (let ((colon (position #\: term)))
      (when (and colon (not (search "://" term)) (> colon 0))
        (let* ((prefix (subseq term 0 (1+ colon)))
               (local (subseq term (1+ colon)))
               (entry (assoc prefix (graph-prefixes g) :test #'equal)))
          (when entry
            (return-from expand-prefix
              (concatenate 'string (cdr entry) local)))))))
  term)

(defun expand-if-prefixed (g term)
  "Expand term if it's a prefixed string, otherwise return as-is."
  (if (stringp term) (expand-prefix g term) term))

(defun register-common-prefixes (g)
  "Register well-known RDF/OWL prefixes."
  (dolist (pair '(("rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
                  ("rdfs" "http://www.w3.org/2000/01/rdf-schema#")
                  ("owl" "http://www.w3.org/2002/07/owl#")
                  ("xsd" "http://www.w3.org/2001/XMLSchema#")
                  ("foaf" "http://xmlns.com/foaf/0.1/")
                  ("dc" "http://purl.org/dc/elements/1.1/")
                  ("dct" "http://purl.org/dc/terms/")
                  ("schema" "http://schema.org/")
                  ("skos" "http://www.w3.org/2004/02/skos/core#")))
    (register-prefix g (first pair) (second pair))))
