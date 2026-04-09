;;;; schema.lisp
;;;; Schema validation, constraint checking, duplicate detection

(in-package #:ariadne)

;;; ==========================================================================
;;; Schema definition
;;; ==========================================================================

(defun graph-schema (g)
  (getf (graph-extra g) :schema))

(defun (setf graph-schema) (val g)
  (setf (getf (graph-extra g) :schema) val))

(defun define-schema (g &key classes)
  "Define a schema for the graph.
CLASSES is a list of (class-name :properties ((pred :type T :required P :min N :max N) ...))."
  (setf (graph-schema g)
        (mapcar (lambda (cls)
                  (destructuring-bind (name &key properties) cls
                    (cons name
                          (mapcar (lambda (prop)
                                    (destructuring-bind (pred &key type required (min 0) max) prop
                                      (list :predicate pred
                                            :type type
                                            :required required
                                            :min (if required (max 1 min) min)
                                            :max max)))
                                  properties))))
                classes)))

;;; ==========================================================================
;;; Validation
;;; ==========================================================================

(defun validate-graph (g)
  "Validate graph against its schema. Returns list of error strings, NIL if valid."
  (let ((schema (graph-schema g))
        (errors nil)
        (rdf-type "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
    (dolist (class-def schema)
      (let ((class-name (car class-def))
            (props (cdr class-def)))
        ;; Find all instances of this class
        (dolist (inst-tr (get-triples g :predicate rdf-type))
          (when (equal (triple-object inst-tr) class-name)
            (let ((entity (triple-subject inst-tr)))
              (dolist (prop-def props)
                (let* ((pred (getf prop-def :predicate))
                       (expected-type (getf prop-def :type))
                       (min-card (getf prop-def :min))
                       (max-card (getf prop-def :max))
                       (values (get-triples g :subject entity :predicate pred))
                       (count (length values)))
                  ;; Min cardinality / required
                  (when (and min-card (> min-card 0) (< count min-card))
                    (push (format nil "~A: required property '~A' missing (min ~A, got ~A)"
                                  entity pred min-card count)
                          errors))
                  ;; Max cardinality
                  (when (and max-card (> count max-card))
                    (push (format nil "~A: cardinality violation on '~A' (max ~A, got ~A)"
                                  entity pred max-card count)
                          errors))
                  ;; Type checking
                  (when expected-type
                    (dolist (v values)
                      (let ((obj (triple-object v)))
                        (unless (type-matches-p obj expected-type)
                          (push (format nil "~A: type mismatch on '~A' — expected ~A, got ~A"
                                        entity pred expected-type (type-of obj))
                                errors))))))))))))
    (nreverse errors)))

(defun type-matches-p (value expected)
  (case expected
    (string (stringp value))
    (number (numberp value))
    (integer (integerp value))
    (symbol (symbolp value))
    (t t)))

;;; ==========================================================================
;;; Duplicate detection
;;; ==========================================================================

(defun string-similarity (a b)
  "Compute Jaccard similarity on character bigrams."
  (let ((ba (bigrams a))
        (bb (bigrams b)))
    (if (and ba bb)
        (let ((intersection (length (intersection ba bb :test #'equal)))
              (union (length (union ba bb :test #'equal))))
          (if (> union 0) (/ intersection union) 0.0))
        0.0)))

(defun bigrams (s)
  (when (> (length s) 1)
    (loop for i below (1- (length s))
          collect (subseq s i (+ i 2)))))

(defun find-similar-entities (g &key predicate (threshold 0.8))
  "Find pairs of entities with similar values for PREDICATE.
Returns list of (entity1 . entity2) pairs above THRESHOLD similarity."
  (let ((entities nil)
        (results nil))
    ;; Collect entity -> value pairs
    (dolist (tr (get-triples g :predicate predicate))
      (when (stringp (triple-object tr))
        (push (cons (triple-subject tr) (triple-object tr)) entities)))
    ;; Compare all pairs
    (loop for rest on entities do
      (let ((a (car rest)))
        (dolist (b (cdr rest))
          (let ((sim (string-similarity (cdr a) (cdr b))))
            (when (>= sim threshold)
              (push (cons (car a) (car b)) results))))))
    results))
