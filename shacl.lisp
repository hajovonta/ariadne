;;;; shacl.lisp
;;;; SHACL (Shapes Constraint Language) validation — W3C Recommendation

(in-package #:ariadne)

(defparameter *sh* "http://www.w3.org/ns/shacl#")
(defparameter *rdf-type* "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
(defparameter *xsd* "http://www.w3.org/2001/XMLSchema#")

(defun sh-uri (name) (concatenate 'string *sh* name))

;;; ==========================================================================
;;; Shape extraction
;;; ==========================================================================

(defun find-shapes (g)
  "Find all NodeShape definitions in the graph."
  (mapcar #'triple-subject
          (get-triples g :predicate *rdf-type* :object (sh-uri "NodeShape"))))

(defun shape-targets (g shape)
  "Return list of focus nodes for SHAPE."
  (let ((nodes nil))
    ;; sh:targetClass
    (dolist (tr (get-triples g :subject shape :predicate (sh-uri "targetClass")))
      (dolist (inst (get-triples g :predicate *rdf-type* :object (triple-object tr)))
        (pushnew (triple-subject inst) nodes :test #'equal)))
    ;; sh:targetNode
    (dolist (tr (get-triples g :subject shape :predicate (sh-uri "targetNode")))
      (pushnew (triple-object tr) nodes :test #'equal))
    ;; sh:targetSubjectsOf
    (dolist (tr (get-triples g :subject shape :predicate (sh-uri "targetSubjectsOf")))
      (dolist (data-tr (get-triples g :predicate (triple-object tr)))
        (pushnew (triple-subject data-tr) nodes :test #'equal)))
    ;; sh:targetObjectsOf
    (dolist (tr (get-triples g :subject shape :predicate (sh-uri "targetObjectsOf")))
      (dolist (data-tr (get-triples g :predicate (triple-object tr)))
        (pushnew (triple-object data-tr) nodes :test #'equal)))
    nodes))

(defun shape-property-shapes (g shape)
  "Return list of property shape URIs for SHAPE."
  (mapcar #'triple-object
          (get-triples g :subject shape :predicate (sh-uri "property"))))

(defun prop-shape-path (g ps)
  (let ((tr (first (get-triples g :subject ps :predicate (sh-uri "path")))))
    (when tr (triple-object tr))))

(defun prop-shape-value (g ps pred)
  (let ((tr (first (get-triples g :subject ps :predicate (sh-uri pred)))))
    (when tr (triple-object tr))))

(defun prop-shape-values (g ps pred)
  (mapcar #'triple-object (get-triples g :subject ps :predicate (sh-uri pred))))

;;; ==========================================================================
;;; Constraint checking
;;; ==========================================================================

(defun check-property-shape (g focus-node prop-shape shape)
  "Check a property shape against a focus node. Returns list of violations."
  (let* ((path (prop-shape-path g prop-shape))
         (values (mapcar #'triple-object (get-triples g :subject focus-node :predicate path)))
         (count (length values))
         (violations nil))
    ;; sh:minCount
    (let ((min-c (prop-shape-value g prop-shape "minCount")))
      (when min-c
        (let ((n (if (stringp min-c) (parse-integer min-c :junk-allowed t) min-c)))
          (when (and n (< count n))
            (push (make-violation focus-node path shape
                                  (format nil "minCount ~A but found ~A" n count))
                  violations)))))
    ;; sh:maxCount
    (let ((max-c (prop-shape-value g prop-shape "maxCount")))
      (when max-c
        (let ((n (if (stringp max-c) (parse-integer max-c :junk-allowed t) max-c)))
          (when (and n (> count n))
            (push (make-violation focus-node path shape
                                  (format nil "maxCount ~A but found ~A" n count))
                  violations)))))
    ;; Per-value constraints
    (dolist (val values)
      ;; sh:datatype
      (let ((dt (prop-shape-value g prop-shape "datatype")))
        (when (and dt (not (value-matches-datatype-p val dt)))
          (push (make-violation focus-node path shape
                                (format nil "expected datatype ~A" dt)
                                :value val)
                violations)))
      ;; sh:pattern
      (let ((pat (prop-shape-value g prop-shape "pattern")))
        (when (and pat (stringp val)
                   (not (cl-ppcre:scan pat val)))
          (push (make-violation focus-node path shape
                                (format nil "does not match pattern ~A" pat)
                                :value val)
                violations)))
      ;; sh:nodeKind
      (let ((nk (prop-shape-value g prop-shape "nodeKind")))
        (when (and nk (not (value-matches-node-kind-p val nk)))
          (push (make-violation focus-node path shape
                                (format nil "expected nodeKind ~A" nk)
                                :value val)
                violations)))
      ;; sh:in
      (let ((allowed (prop-shape-values g prop-shape "in")))
        (when (and allowed (not (member val allowed :test #'equal)))
          (push (make-violation focus-node path shape
                                (format nil "value not in allowed set")
                                :value val)
                violations)))
      ;; sh:minInclusive
      (let ((limit (prop-shape-value g prop-shape "minInclusive")))
        (when (and limit (numberp val) (numberp limit) (< val limit))
          (push (make-violation focus-node path shape
                                (format nil "value ~A < minInclusive ~A" val limit)
                                :value val)
                violations)))
      ;; sh:maxInclusive
      (let ((limit (prop-shape-value g prop-shape "maxInclusive")))
        (when (and limit (numberp val) (numberp limit) (> val limit))
          (push (make-violation focus-node path shape
                                (format nil "value ~A > maxInclusive ~A" val limit)
                                :value val)
                violations)))
      ;; sh:minExclusive
      (let ((limit (prop-shape-value g prop-shape "minExclusive")))
        (when (and limit (numberp val) (numberp limit) (<= val limit))
          (push (make-violation focus-node path shape
                                (format nil "value ~A <= minExclusive ~A" val limit)
                                :value val)
                violations)))
      ;; sh:maxExclusive
      (let ((limit (prop-shape-value g prop-shape "maxExclusive")))
        (when (and limit (numberp val) (numberp limit) (>= val limit))
          (push (make-violation focus-node path shape
                                (format nil "value ~A >= maxExclusive ~A" val limit)
                                :value val)
                violations)))
      ;; sh:minLength
      (let ((min-l (prop-shape-value g prop-shape "minLength")))
        (when (and min-l (stringp val))
          (let ((n (if (numberp min-l) min-l (parse-integer (princ-to-string min-l) :junk-allowed t))))
            (when (and n (< (length val) n))
              (push (make-violation focus-node path shape
                                    (format nil "length ~A < minLength ~A" (length val) n)
                                    :value val)
                    violations)))))
      ;; sh:maxLength
      (let ((max-l (prop-shape-value g prop-shape "maxLength")))
        (when (and max-l (stringp val))
          (let ((n (if (numberp max-l) max-l (parse-integer (princ-to-string max-l) :junk-allowed t))))
            (when (and n (> (length val) n))
              (push (make-violation focus-node path shape
                                    (format nil "length ~A > maxLength ~A" (length val) n)
                                    :value val)
                    violations)))))
      ;; sh:hasValue
      (let ((required (prop-shape-value g prop-shape "hasValue")))
        (when (and required (not (member required values :test #'equal)))
          (push (make-violation focus-node path shape
                                (format nil "missing required value ~A" required))
                violations)))
      ;; sh:class
      (let ((cls (prop-shape-value g prop-shape "class")))
        (when (and cls (stringp val))
          (unless (has-triple-p g val *rdf-type* cls)
            (push (make-violation focus-node path shape
                                  (format nil "~A is not an instance of ~A" val cls)
                                  :value val)
                  violations))))
      ;; sh:not — value must NOT satisfy the sub-shape
      (let ((not-shape (prop-shape-value g prop-shape "not")))
        (when (and not-shape (check-value-against-subshape g val not-shape))
          (push (make-violation focus-node path shape
                                "value satisfies sh:not constraint (should not)"
                                :value val)
                violations)))
      ;; sh:and — value must satisfy ALL sub-shapes
      (let ((and-shapes (prop-shape-values g prop-shape "and")))
        (when and-shapes
          (unless (every (lambda (ss) (check-value-against-subshape g val ss)) and-shapes)
            (push (make-violation focus-node path shape
                                  "value does not satisfy all sh:and constraints"
                                  :value val)
                  violations))))
      ;; sh:or — value must satisfy AT LEAST ONE sub-shape
      (let ((or-shapes (prop-shape-values g prop-shape "or")))
        (when or-shapes
          (unless (some (lambda (ss) (check-value-against-subshape g val ss)) or-shapes)
            (push (make-violation focus-node path shape
                                  "value does not satisfy any sh:or constraint"
                                  :value val)
                  violations))))
      ;; sh:xone — value must satisfy EXACTLY ONE sub-shape
      (let ((xone-shapes (prop-shape-values g prop-shape "xone")))
        (when xone-shapes
          (let ((pass-count (count-if (lambda (ss) (check-value-against-subshape g val ss))
                                      xone-shapes)))
            (unless (= 1 pass-count)
              (push (make-violation focus-node path shape
                                    (format nil "sh:xone expects exactly 1 match, got ~A" pass-count)
                                    :value val)
                    violations))))))
    violations))

(defun make-violation (focus-node path shape message &key value)
  (list :focus-node focus-node
        :result-path path
        :source-shape shape
        :result-message message
        :value value
        :result-severity (sh-uri "Violation")))

(defun value-matches-datatype-p (val datatype)
  "Check if VAL matches the expected XSD datatype."
  (let ((dt-local (subseq datatype (length *xsd*))))
    (cond
      ((equal dt-local "integer")
       (or (integerp val)
           (and (stringp val) (every #'digit-char-p val) (> (length val) 0))))
      ((equal dt-local "decimal")
       (or (numberp val)
           (and (stringp val) (cl-ppcre:scan "^-?[0-9]+(\\.[0-9]+)?$" val))))
      ((equal dt-local "string") (stringp val))
      ((equal dt-local "boolean")
       (or (member val '(t nil))
           (member val '("true" "false") :test #'equal)))
      (t t))))

(defun value-matches-node-kind-p (val node-kind)
  "Check if VAL matches the expected sh:nodeKind."
  (cond
    ((equal node-kind (sh-uri "IRI"))
     (and (stringp val) (search "://" val)))
    ((equal node-kind (sh-uri "Literal"))
     (or (stringp val) (numberp val)))
    ((equal node-kind (sh-uri "BlankNode"))
     (and (stringp val) (>= (length val) 2)
          (char= #\_ (char val 0)) (char= #\: (char val 1))))
    (t t)))

;;; ==========================================================================
;;; Sub-shape checking (for logical operators)
;;; ==========================================================================

(defun check-value-against-subshape (g val sub-shape)
  "Check a single value against a sub-shape's constraints. Returns T if valid."
  (let ((dt (prop-shape-value g sub-shape "datatype"))
        (pat (prop-shape-value g sub-shape "pattern"))
        (nk (prop-shape-value g sub-shape "nodeKind"))
        (mini (prop-shape-value g sub-shape "minInclusive"))
        (maxi (prop-shape-value g sub-shape "maxInclusive"))
        (mine (prop-shape-value g sub-shape "minExclusive"))
        (maxe (prop-shape-value g sub-shape "maxExclusive")))
    (and (or (null dt) (value-matches-datatype-p val dt))
         (or (null pat) (not (stringp val)) (cl-ppcre:scan pat val))
         (or (null nk) (value-matches-node-kind-p val nk))
         (or (null mini) (not (numberp val)) (not (numberp mini)) (>= val mini))
         (or (null maxi) (not (numberp val)) (not (numberp maxi)) (<= val maxi))
         (or (null mine) (not (numberp val)) (not (numberp mine)) (> val mine))
         (or (null maxe) (not (numberp val)) (not (numberp maxe)) (< val maxe)))))

;;; ==========================================================================
;;; Main validation entry point
;;; ==========================================================================

(defun shacl-validate (g)
  "Validate graph G against all SHACL shapes defined within it.
Returns a plist with :conforms (boolean) and :results (list of violations)."
  (let ((shapes (find-shapes g))
        (all-violations nil))
    (dolist (shape shapes)
      (let ((targets (shape-targets g shape))
            (prop-shapes (shape-property-shapes g shape)))
        (dolist (focus targets)
          (dolist (ps prop-shapes)
            (let ((violations (check-property-shape g focus ps shape)))
              (setf all-violations (nconc all-violations violations)))))))
    (list :conforms (null all-violations)
          :results all-violations)))
