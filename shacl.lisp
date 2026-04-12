;;;; shacl.lisp
;;;; SHACL (Shapes Constraint Language) validation — W3C Recommendation

(in-package #:ariadne)

(defparameter *sh* "http://www.w3.org/ns/shacl#")
(defparameter *rdf-type* "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
(defparameter *xsd* "http://www.w3.org/2001/XMLSchema#")

(defun sh-uri (name) (concatenate 'string *sh* name))

(defun lit-val (x)
  "Unwrap rdf-literal to its value, or return x as-is."
  (if (rdf-literal-p x) (rdf-literal-value x) x))

(defun shacl-value< (a b)
  "Compare two SHACL values. Handles numbers, strings, and timestamps."
  (let ((a (lit-val a)) (b (lit-val b)))
    (cond
      ((and (numberp a) (numberp b)) (< a b))
      ((and (stringp a) (stringp b)) (string< a b))
      ((and (typep a 'local-time:timestamp) (typep b 'local-time:timestamp))
       (local-time:timestamp< a b))
      (t nil))))

(defun shacl-value<= (a b)
  (let ((a (lit-val a)) (b (lit-val b)))
    (cond
      ((and (numberp a) (numberp b)) (<= a b))
      ((and (stringp a) (stringp b)) (string<= a b))
      ((and (typep a 'local-time:timestamp) (typep b 'local-time:timestamp))
       (or (local-time:timestamp< a b) (local-time:timestamp= a b)))
      (t nil))))

(defun shacl-value>= (a b) (shacl-value<= b a))
(defun shacl-value> (a b) (shacl-value< b a))

(defun all-subclasses (g class)
  "Return all classes that are rdfs:subClassOf CLASS (transitive)."
  (let ((result nil)
        (rdfs-subclass "http://www.w3.org/2000/01/rdf-schema#subClassOf"))
    (labels ((walk (c)
               (dolist (tr (get-triples g :predicate rdfs-subclass :object c))
                 (let ((sub (triple-subject tr)))
                   (unless (member sub result :test #'equal)
                     (push sub result)
                     (walk sub))))))
      (walk class))
    result))

;;; ==========================================================================
;;; Shape extraction
;;; ==========================================================================

(defun find-shapes (g)
  "Find all shape definitions in the graph (explicit and implicit)."
  (let ((shapes nil))
    ;; Explicit types
    (dolist (tr (get-triples g :predicate *rdf-type* :object (sh-uri "NodeShape")))
      (pushnew (triple-subject tr) shapes :test #'equal))
    (dolist (tr (get-triples g :predicate *rdf-type* :object (sh-uri "PropertyShape")))
      (pushnew (triple-subject tr) shapes :test #'equal))
    ;; Implicit: anything with sh:targetClass, sh:targetNode, sh:targetSubjectsOf, sh:targetObjectsOf
    (dolist (pred '("targetClass" "targetNode" "targetSubjectsOf" "targetObjectsOf"))
      (dolist (tr (get-triples g :predicate (sh-uri pred)))
        (pushnew (triple-subject tr) shapes :test #'equal)))
    shapes))

(defun shape-targets (g shape)
  "Return list of focus nodes for SHAPE."
  (let ((nodes nil))
    ;; sh:targetClass (including subclasses)
    (dolist (tr (get-triples g :subject shape :predicate (sh-uri "targetClass")))
      (let ((cls (triple-object tr)))
        (dolist (inst (get-triples g :predicate *rdf-type* :object cls))
          (pushnew (triple-subject inst) nodes :test #'equal))
        (dolist (sc (all-subclasses g cls))
          (dolist (inst (get-triples g :predicate *rdf-type* :object sc))
            (pushnew (triple-subject inst) nodes :test #'equal)))))
    ;; Implicit target class: shape is also an rdfs:Class or owl:Class
    (when (or (has-triple-p g shape *rdf-type* "http://www.w3.org/2000/01/rdf-schema#Class")
              (has-triple-p g shape *rdf-type* "http://www.w3.org/2002/07/owl#Class"))
      ;; Direct instances
      (dolist (inst (get-triples g :predicate *rdf-type* :object shape))
        (pushnew (triple-subject inst) nodes :test #'equal))
      ;; Instances of subclasses
      (let ((subclasses (all-subclasses g shape)))
        (dolist (sc subclasses)
          (dolist (inst (get-triples g :predicate *rdf-type* :object sc))
            (pushnew (triple-subject inst) nodes :test #'equal)))))
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

(defun resolve-path-values (g focus-node path)
  "Resolve values for a SHACL property path from focus-node.
PATH can be a simple URI or a blank node with path operators."
  (cond
    ;; Simple predicate path
    ((and (stringp path) (not (eql 0 (search "_:" path))))
     (mapcar #'triple-object (get-triples g :subject focus-node :predicate path)))
    ;; Complex path (blank node)
    ((stringp path)
     (let ((has-first (first (get-triples g :subject path :predicate "http://www.w3.org/1999/02/22-rdf-syntax-ns#first")))
           (inverse (first (get-triples g :subject path :predicate (sh-uri "inversePath"))))
           (alt-list (first (get-triples g :subject path :predicate (sh-uri "alternativePath"))))
           (zero-more (first (get-triples g :subject path :predicate (sh-uri "zeroOrMorePath"))))
           (one-more (first (get-triples g :subject path :predicate (sh-uri "oneOrMorePath"))))
           (zero-one (first (get-triples g :subject path :predicate (sh-uri "zeroOrOnePath")))))
       (cond
         ;; Sequence path (RDF list) — check first, takes priority
         (has-first
          (let ((steps (rdf-list-to-list g path)))
            (sequence-path-values g (list focus-node) steps)))
         ;; sh:inversePath
         (inverse
          (let ((pred (triple-object inverse)))
            (mapcar #'triple-subject (get-triples g :predicate pred :object focus-node))))
         ;; sh:alternativePath (RDF list of paths)
         (alt-list
          (let ((paths (rdf-list-to-list g (triple-object alt-list)))
                (results nil))
            (dolist (p paths)
              (setf results (nconc results (resolve-path-values g focus-node p))))
            results))
         ;; sh:zeroOrMorePath
         (zero-more
          (let ((pred (triple-object zero-more)))
            (transitive-path-values g focus-node pred t)))
         ;; sh:oneOrMorePath
         (one-more
          (let ((pred (triple-object one-more)))
            (transitive-path-values g focus-node pred nil)))
         ;; sh:zeroOrOnePath
         (zero-one
          (let ((pred (triple-object zero-one)))
            (cons focus-node (mapcar #'triple-object (get-triples g :subject focus-node :predicate pred)))))
         ;; Unknown — treat as simple
         (t (mapcar #'triple-object (get-triples g :subject focus-node :predicate path))))))
    (t nil)))

(defun transitive-path-values (g start pred include-self)
  "Follow predicate transitively. If INCLUDE-SELF, include the start node."
  (let ((visited (make-hash-table :test 'equal))
        (result nil))
    (when include-self (push start result) (setf (gethash start visited) t))
    (labels ((walk (node)
               (dolist (tr (get-triples g :subject node :predicate pred))
                 (let ((obj (triple-object tr)))
                   (unless (gethash obj visited)
                     (setf (gethash obj visited) t)
                     (push obj result)
                     (walk obj))))))
      (walk start))
    result))

(defun sequence-path-values (g nodes steps)
  "Follow a sequence of path steps."
  (dolist (step steps)
    (let ((next nil))
      (dolist (node nodes)
        (setf next (nconc next (resolve-path-values g node step))))
      (setf nodes next)))
  nodes)

(defun prop-shape-value (g ps pred)
  (let ((tr (first (get-triples g :subject ps :predicate (sh-uri pred)))))
    (when tr (triple-object tr))))

(defun prop-shape-values (g ps pred)
  (mapcar #'triple-object (get-triples g :subject ps :predicate (sh-uri pred))))

(defun rdf-list-to-list (g head)
  "Follow an RDF list (rdf:first/rdf:rest) and return CL list of values."
  (let ((result nil)
        (node head)
        (rdf-first "http://www.w3.org/1999/02/22-rdf-syntax-ns#first")
        (rdf-rest "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest")
        (rdf-nil "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))
    (loop while (and node (not (equal node rdf-nil))) do
      (let ((first-tr (first (get-triples g :subject node :predicate rdf-first)))
            (rest-tr (first (get-triples g :subject node :predicate rdf-rest))))
        (when first-tr (push (triple-object first-tr) result))
        (setf node (when rest-tr (triple-object rest-tr)))))
    (nreverse result)))

(defun prop-shape-list-value (g ps pred)
  "Get the value of PRED on PS. If it points to an RDF list, expand it."
  (let ((tr (first (get-triples g :subject ps :predicate (sh-uri pred)))))
    (when tr
      (let ((obj (triple-object tr)))
        ;; Check if it's an RDF list head (has rdf:first)
        (if (get-triples g :subject obj :predicate "http://www.w3.org/1999/02/22-rdf-syntax-ns#first")
            (rdf-list-to-list g obj)
            ;; Multiple direct values
            (mapcar #'triple-object (get-triples g :subject ps :predicate (sh-uri pred))))))))

;;; ==========================================================================
;;; Constraint checking
;;; ==========================================================================

(defun check-property-shape (g focus-node prop-shape shape)
  "Check a property shape against a focus node. Returns list of violations."
  (let* ((path (prop-shape-path g prop-shape))
         (values (resolve-path-values g focus-node path))
         (count (length values))
         (violations nil))
    ;; sh:minCount
    (let ((min-c (lit-val (prop-shape-value g prop-shape "minCount"))))
      (when min-c
        (let ((n (if (stringp min-c) (parse-integer min-c :junk-allowed t) min-c)))
          (when (and n (< count n))
            (push (make-violation focus-node path shape
                                  (format nil "minCount ~A but found ~A" n count))
                  violations)))))
    ;; sh:maxCount
    (let ((max-c (lit-val (prop-shape-value g prop-shape "maxCount"))))
      (when max-c
        (let ((n (if (stringp max-c) (parse-integer max-c :junk-allowed t) max-c)))
          (when (and n (> count n))
            (push (make-violation focus-node path shape
                                  (format nil "maxCount ~A but found ~A" n count))
                  violations)))))
    ;; Per-value constraints
    (dolist (val values)
      (let ((sval (lit-val val)))
      ;; sh:datatype
      (let ((dt (prop-shape-value g prop-shape "datatype")))
        (when (and dt (not (value-matches-datatype-p val dt)))
          (push (make-violation focus-node path shape
                                (format nil "expected datatype ~A" dt)
                                :value val)
                violations)))
      ;; sh:pattern
      (let ((pat (lit-val (prop-shape-value g prop-shape "pattern")))
            (flags (lit-val (prop-shape-value g prop-shape "flags"))))
        (when (and pat (stringp sval))
          (let ((scanner (if (and flags (search "i" flags))
                             (cl-ppcre:create-scanner pat :case-insensitive-mode t)
                             pat)))
            (unless (cl-ppcre:scan scanner sval)
              (push (make-violation focus-node path shape
                                    (format nil "does not match pattern ~A" pat)
                                    :value val)
                    violations)))))
      ;; sh:nodeKind
      (let ((nk (prop-shape-value g prop-shape "nodeKind")))
        (when (and nk (not (value-matches-node-kind-p val nk)))
          (push (make-violation focus-node path shape
                                (format nil "expected nodeKind ~A" nk)
                                :value val)
                violations)))
      ;; sh:in
      (let ((allowed (prop-shape-list-value g prop-shape "in")))
        (when (and allowed (not (member val allowed :test #'equal)))
          (push (make-violation focus-node path shape
                                (format nil "value not in allowed set")
                                :value val)
                violations)))
      ;; sh:minInclusive
      (let ((limit (prop-shape-value g prop-shape "minInclusive")))
        (when (and limit (not (shacl-value>= val limit)))
          (push (make-violation focus-node path shape
                                (format nil "value ~A < minInclusive ~A" val limit)
                                :value val)
                violations)))
      ;; sh:maxInclusive
      (let ((limit (prop-shape-value g prop-shape "maxInclusive")))
        (when (and limit (not (shacl-value<= val limit)))
          (push (make-violation focus-node path shape
                                (format nil "value ~A > maxInclusive ~A" val limit)
                                :value val)
                violations)))
      ;; sh:minExclusive
      (let ((limit (prop-shape-value g prop-shape "minExclusive")))
        (when (and limit (not (shacl-value> val limit)))
          (push (make-violation focus-node path shape
                                (format nil "value ~A <= minExclusive ~A" val limit)
                                :value val)
                violations)))
      ;; sh:maxExclusive
      (let ((limit (prop-shape-value g prop-shape "maxExclusive")))
        (when (and limit (not (shacl-value< val limit)))
          (push (make-violation focus-node path shape
                                (format nil "value ~A >= maxExclusive ~A" val limit)
                                :value val)
                violations)))
      ;; sh:minLength
      (let ((min-l (prop-shape-value g prop-shape "minLength")))
        (when (and min-l (stringp sval))
          (let ((n (if (numberp min-l) min-l (parse-integer (princ-to-string min-l) :junk-allowed t))))
            (when (and n (< (length sval) n))
              (push (make-violation focus-node path shape
                                    (format nil "length ~A < minLength ~A" (length sval) n)
                                    :value val)
                    violations)))))
      ;; sh:maxLength
      (let ((max-l (prop-shape-value g prop-shape "maxLength")))
        (when (and max-l (stringp sval))
          (let ((n (if (numberp max-l) max-l (parse-integer (princ-to-string max-l) :junk-allowed t))))
            (when (and n (> (length sval) n))
              (push (make-violation focus-node path shape
                                    (format nil "length ~A > maxLength ~A" (length sval) n)
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
        (when (and cls (stringp sval))
          (unless (has-triple-p g sval *rdf-type* cls)
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
      (let ((and-shapes (prop-shape-list-value g prop-shape "and")))
        (when and-shapes
          (unless (every (lambda (ss) (check-value-against-subshape g val ss)) and-shapes)
            (push (make-violation focus-node path shape
                                  "value does not satisfy all sh:and constraints"
                                  :value val)
                  violations))))
      ;; sh:or — value must satisfy AT LEAST ONE sub-shape
      (let ((or-shapes (prop-shape-list-value g prop-shape "or")))
        (when or-shapes
          (unless (some (lambda (ss) (check-value-against-subshape g val ss)) or-shapes)
            (push (make-violation focus-node path shape
                                  "value does not satisfy any sh:or constraint"
                                  :value val)
                  violations))))
      ;; sh:xone — value must satisfy EXACTLY ONE sub-shape
      (let ((xone-shapes (prop-shape-list-value g prop-shape "xone")))
        (when xone-shapes
          (let ((pass-count (count-if (lambda (ss) (check-value-against-subshape g val ss))
                                      xone-shapes)))
            (unless (= 1 pass-count)
              (push (make-violation focus-node path shape
                                    (format nil "sh:xone expects exactly 1 match, got ~A" pass-count)
                                    :value val)
                    violations)))))))
    ;; sh:equals
    (let ((eq-path (prop-shape-value g prop-shape "equals")))
      (when eq-path
        (let ((other-vals (mapcar #'triple-object (get-triples g :subject focus-node :predicate eq-path))))
          (unless (and (null (set-difference values other-vals :test #'equal))
                       (null (set-difference other-vals values :test #'equal)))
            (push (make-violation focus-node path shape
                                  (format nil "values not equal to ~A" eq-path))
                  violations)))))
    ;; sh:disjoint
    (let ((disj-path (prop-shape-value g prop-shape "disjoint")))
      (when disj-path
        (let ((other-vals (mapcar #'triple-object (get-triples g :subject focus-node :predicate disj-path))))
          (dolist (val values)
            (when (member val other-vals :test #'equal)
              (push (make-violation focus-node path shape
                                    (format nil "value ~A also in ~A" val disj-path)
                                    :value val)
                    violations))))))
    ;; sh:lessThan
    (let ((lt-path (prop-shape-value g prop-shape "lessThan")))
      (when lt-path
        (let ((other-vals (mapcar #'triple-object (get-triples g :subject focus-node :predicate lt-path))))
          (dolist (val values)
            (dolist (ov other-vals)
              (unless (shacl-value< val ov)
                (push (make-violation focus-node path shape
                                      (format nil "~A not < ~A" val ov) :value val)
                      violations)))))))
    ;; sh:lessThanOrEquals
    (let ((lte-path (prop-shape-value g prop-shape "lessThanOrEquals")))
      (when lte-path
        (let ((other-vals (mapcar #'triple-object (get-triples g :subject focus-node :predicate lte-path))))
          (dolist (val values)
            (dolist (ov other-vals)
              (unless (shacl-value<= val ov)
                (push (make-violation focus-node path shape
                                      (format nil "~A not <= ~A" val ov) :value val)
                      violations)))))))
    ;; sh:uniqueLang
    (let ((ul (lit-val (prop-shape-value g prop-shape "uniqueLang"))))
      (when (or (eq ul t) (equal ul "true"))
        (let ((seen nil))
          (dolist (val values)
            (let ((lang (when (rdf-literal-p val) (rdf-literal-language val))))
              (when lang
                (if (member lang seen :test #'string-equal)
                    (push (make-violation focus-node path shape
                                          (format nil "duplicate language: ~A" lang)
                                          :value val)
                          violations)
                    (push lang seen))))))))
    ;; sh:qualifiedValueShape
    (let ((qvs (prop-shape-value g prop-shape "qualifiedValueShape")))
      (when qvs
        (let* ((qmin (lit-val (prop-shape-value g prop-shape "qualifiedMinCount")))
               (qmax (lit-val (prop-shape-value g prop-shape "qualifiedMaxCount")))
               (disjoint-p (let ((d (lit-val (prop-shape-value g prop-shape "qualifiedValueShapesDisjoint"))))
                              (or (eq d t) (equal d "true"))))
               ;; Find sibling qualified shapes (same parent, same path, different qualifiedValueShape)
               (sibling-qvs (when disjoint-p
                               (let ((parent shape)
                                     (siblings nil))
                                 (dolist (ps (shape-property-shapes g parent))
                                   (when (and (not (equal ps prop-shape))
                                              (equal (prop-shape-path g ps) path))
                                     (let ((s-qvs (prop-shape-value g ps "qualifiedValueShape")))
                                       (when s-qvs (push s-qvs siblings)))))
                                 siblings)))
               (conforming (count-if (lambda (val)
                                       (and (check-value-against-subshape g val qvs)
                                            ;; If disjoint, exclude values matching siblings
                                            (or (not disjoint-p)
                                                (not (some (lambda (sib)
                                                             (check-value-against-subshape g val sib))
                                                           sibling-qvs)))))
                                     values)))
          (when qmin
            (let ((n (if (numberp qmin) qmin (parse-integer (princ-to-string qmin) :junk-allowed t))))
              (when (and n (< conforming n))
                (push (make-violation focus-node path shape
                                      (format nil "qualifiedMinCount ~A but ~A conform" n conforming))
                      violations))))
          (when qmax
            (let ((n (if (numberp qmax) qmax (parse-integer (princ-to-string qmax) :junk-allowed t))))
              (when (and n (> conforming n))
                (push (make-violation focus-node path shape
                                      (format nil "qualifiedMaxCount ~A but ~A conform" n conforming))
                      violations)))))))
    ;; sh:languageIn
    (let ((lang-list (prop-shape-list-value g prop-shape "languageIn")))
      (when lang-list
        (dolist (val values)
          (let ((lang (when (rdf-literal-p val) (rdf-literal-language val))))
            (if lang
                (unless (some (lambda (allowed)
                                (let ((allowed (lit-val allowed)))
                                  (or (string-equal lang allowed)
                                      (and (> (length lang) (length allowed))
                                           (char= #\- (char lang (length allowed)))
                                           (string-equal (subseq lang 0 (length allowed)) allowed)))))
                              lang-list)
                  (push (make-violation focus-node path shape
                                        (format nil "language ~A not in ~S" lang lang-list)
                                        :value val)
                        violations))
                (when (rdf-literal-p val)
                  (push (make-violation focus-node path shape
                                        "value has no language tag" :value val)
                        violations)))))))
    ;; sh:node
    (let ((node-shape (prop-shape-value g prop-shape "node")))
      (when node-shape
        (dolist (val values)
          (unless (check-value-against-subshape g val node-shape)
            (push (make-violation focus-node path shape
                                  (format nil "does not conform to ~A" node-shape)
                                  :value val)
                  violations)))))
    ;; Nested sh:property — validate each value against nested property shapes
    (let ((nested-props (shape-property-shapes g prop-shape)))
      (when nested-props
        (dolist (val values)
          (dolist (nps nested-props)
            (let ((nested-violations (check-property-shape g val nps shape)))
              (setf violations (nconc violations nested-violations)))))))
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
  (if (rdf-literal-p val)
      (equal (rdf-literal-datatype val) datatype)
      (let ((v (lit-val val)))
        (cond
          ((search "integer" datatype) (integerp v))
          ((search "decimal" datatype) (numberp v))
          ((search "float" datatype) (numberp v))
          ((search "double" datatype) (numberp v))
          ((equal datatype (concatenate 'string *xsd* "string")) (stringp v))
          ((equal datatype (concatenate 'string *xsd* "boolean")) (member v '(t nil)))
          ((search "dateTime" datatype) (typep v 'local-time:timestamp))
          (t t)))))

(defun value-matches-node-kind-p (val node-kind)
  "Check if VAL matches the expected sh:nodeKind."
  (cond
    ((equal node-kind (sh-uri "IRI"))
     (and (stringp val) (not (rdf-literal-p val)) (search "://" val)))
    ((equal node-kind (sh-uri "Literal"))
     (or (rdf-literal-p val) (numberp val)))
    ((equal node-kind (sh-uri "BlankNode"))
     (and (stringp val) (not (rdf-literal-p val))
          (>= (length val) 2)
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
        (maxe (prop-shape-value g sub-shape "maxExclusive"))
        (cls (prop-shape-value g sub-shape "class"))
        (hv (prop-shape-value g sub-shape "hasValue"))
        (in-list (prop-shape-list-value g sub-shape "in"))
        (not-shape (prop-shape-value g sub-shape "not"))
        (and-shapes (prop-shape-list-value g sub-shape "and"))
        (or-shapes (prop-shape-list-value g sub-shape "or"))
        (xone-shapes (prop-shape-list-value g sub-shape "xone"))
        (prop-shapes (shape-property-shapes g sub-shape))
        (closed (lit-val (prop-shape-value g sub-shape "closed")))
        (node-ref (prop-shape-value g sub-shape "node")))
    (and (or (null dt) (value-matches-datatype-p val dt))
         (or (null pat) (not (stringp (lit-val val))) (cl-ppcre:scan (lit-val pat) (lit-val val)))
         (or (null nk) (value-matches-node-kind-p val nk))
         (or (null mini) (shacl-value>= val mini))
         (or (null maxi) (shacl-value<= val maxi))
         (or (null mine) (shacl-value> val mine))
         (or (null maxe) (shacl-value< val maxe))
         (or (null cls) (and (stringp val) (has-triple-p g val *rdf-type* cls)))
         (or (null hv) (equal val hv))
         (or (null in-list) (member val in-list :test #'equal))
         (or (null not-shape) (not (check-value-against-subshape g val not-shape)))
         (or (null and-shapes) (every (lambda (ss) (check-value-against-subshape g val ss)) and-shapes))
         (or (null or-shapes) (some (lambda (ss) (check-value-against-subshape g val ss)) or-shapes))
         (or (null xone-shapes) (= 1 (count-if (lambda (ss) (check-value-against-subshape g val ss)) xone-shapes)))
         (or (null prop-shapes)
             (every (lambda (ps)
                      (null (check-property-shape g val ps sub-shape)))
                    prop-shapes))
         ;; sh:closed
         (or (not (or (eq closed t) (equal closed "true")))
             (let ((allowed (mapcar (lambda (ps) (prop-shape-path g ps)) prop-shapes))
                   (ignored (prop-shape-list-value g sub-shape "ignoredProperties")))
               (dolist (ig ignored) (push ig allowed))
               (push *rdf-type* allowed)
               (every (lambda (tr) (member (triple-predicate tr) allowed :test #'equal))
                      (get-triples g :subject val))))
         ;; sh:node (recursive)
         (or (null node-ref) (check-value-against-subshape g val node-ref)))))

;;; ==========================================================================
;;; Node-level constraint checking
;;; ==========================================================================

(defun check-node-constraints (g focus-node shape)
  "Check constraints placed directly on a NodeShape against the focus node."
  (let ((violations nil)
        (val focus-node)
        (sval (lit-val focus-node)))
    ;; sh:class — focus node must be instance of class
    (let ((cls (prop-shape-value g shape "class")))
      (when (and cls (not (has-triple-p g focus-node *rdf-type* cls)))
        (push (make-violation focus-node nil shape
                              (format nil "not an instance of ~A" cls))
              violations)))
    ;; sh:datatype
    (let ((dt (prop-shape-value g shape "datatype")))
      (when (and dt (not (value-matches-datatype-p val dt)))
        (push (make-violation focus-node nil shape
                              (format nil "expected datatype ~A" dt))
              violations)))
    ;; sh:nodeKind
    (let ((nk (prop-shape-value g shape "nodeKind")))
      (when (and nk (not (value-matches-node-kind-p val nk)))
        (push (make-violation focus-node nil shape
                              (format nil "expected nodeKind ~A" nk))
              violations)))
    ;; sh:in
    (let ((allowed (prop-shape-list-value g shape "in")))
      (when (and allowed (not (member val allowed :test #'equal)))
        (push (make-violation focus-node nil shape "value not in allowed set")
              violations)))
    ;; sh:hasValue — the focus node's values must include this
    (let ((required (prop-shape-value g shape "hasValue")))
      (when (and required (not (equal val required)))
        (push (make-violation focus-node nil shape
                              (format nil "expected hasValue ~A" required))
              violations)))
    ;; sh:pattern
    (let ((pat (lit-val (prop-shape-value g shape "pattern")))
          (flags (lit-val (prop-shape-value g shape "flags"))))
      (when (and pat (stringp sval))
        (let ((scanner (if (and flags (search "i" flags))
                           (cl-ppcre:create-scanner pat :case-insensitive-mode t)
                           pat)))
          (unless (cl-ppcre:scan scanner sval)
            (push (make-violation focus-node nil shape
                                  (format nil "does not match pattern ~A" pat))
                  violations)))))
    ;; sh:minInclusive/maxInclusive/minExclusive/maxExclusive
    (let ((mini (prop-shape-value g shape "minInclusive")))
      (when (and mini (not (shacl-value>= val mini)))
        (push (make-violation focus-node nil shape
                              (format nil "value < minInclusive ~A" mini))
              violations)))
    (let ((maxi (prop-shape-value g shape "maxInclusive")))
      (when (and maxi (not (shacl-value<= val maxi)))
        (push (make-violation focus-node nil shape
                              (format nil "value > maxInclusive ~A" maxi))
              violations)))
    (let ((mine (prop-shape-value g shape "minExclusive")))
      (when (and mine (not (shacl-value> val mine)))
        (push (make-violation focus-node nil shape
                              (format nil "value <= minExclusive ~A" mine))
              violations)))
    (let ((maxe (prop-shape-value g shape "maxExclusive")))
      (when (and maxe (not (shacl-value< val maxe)))
        (push (make-violation focus-node nil shape
                              (format nil "value >= maxExclusive ~A" maxe))
              violations)))
    ;; sh:minLength/maxLength
    (when (stringp sval)
      (let ((min-l (lit-val (prop-shape-value g shape "minLength"))))
        (when min-l
          (let ((n (if (numberp min-l) min-l (parse-integer (princ-to-string min-l) :junk-allowed t))))
            (when (and n (< (length sval) n))
              (push (make-violation focus-node nil shape
                                    (format nil "length < minLength ~A" n))
                    violations)))))
      (let ((max-l (lit-val (prop-shape-value g shape "maxLength"))))
        (when max-l
          (let ((n (if (numberp max-l) max-l (parse-integer (princ-to-string max-l) :junk-allowed t))))
            (when (and n (> (length sval) n))
              (push (make-violation focus-node nil shape
                                    (format nil "length > maxLength ~A" n))
                    violations))))))
    ;; sh:not
    (let ((not-shape (prop-shape-value g shape "not")))
      (when (and not-shape (check-value-against-subshape g val not-shape))
        (push (make-violation focus-node nil shape "satisfies sh:not (should not)")
              violations)))
    ;; sh:and
    (let ((and-shapes (prop-shape-list-value g shape "and")))
      (when and-shapes
        (unless (every (lambda (ss) (check-value-against-subshape g val ss)) and-shapes)
          (push (make-violation focus-node nil shape "does not satisfy all sh:and")
                violations))))
    ;; sh:or
    (let ((or-shapes (prop-shape-list-value g shape "or")))
      (when or-shapes
        (unless (some (lambda (ss) (check-value-against-subshape g val ss)) or-shapes)
          (push (make-violation focus-node nil shape "does not satisfy any sh:or")
                violations))))
    ;; sh:xone
    (let ((xone-shapes (prop-shape-list-value g shape "xone")))
      (when xone-shapes
        (let ((pass-count (count-if (lambda (ss) (check-value-against-subshape g val ss))
                                    xone-shapes)))
          (unless (= 1 pass-count)
            (push (make-violation focus-node nil shape
                                  (format nil "sh:xone expects 1 match, got ~A" pass-count))
                  violations)))))
    ;; sh:languageIn (node level)
    (let ((lang-list (prop-shape-list-value g shape "languageIn")))
      (when lang-list
        (when (stringp val)
          (let ((at (position #\@ val)))
            (if at
                (let ((lang (subseq val (1+ at))))
                  (unless (some (lambda (allowed)
                                  (or (string-equal lang allowed)
                                      (and (> (length lang) (length allowed))
                                           (char= #\- (char lang (length allowed)))
                                           (string-equal (subseq lang 0 (length allowed)) allowed))))
                                lang-list)
                    (push (make-violation focus-node nil shape
                                          (format nil "language ~A not in ~S" lang lang-list))
                          violations)))
                (push (make-violation focus-node nil shape "no language tag")
                      violations))))))
    ;; sh:closed
    (let ((closed (lit-val (prop-shape-value g shape "closed"))))
      (when (or (eq closed t) (equal closed "true"))
        (let ((allowed-preds (mapcar (lambda (ps) (prop-shape-path g ps))
                                     (shape-property-shapes g shape)))
              (ignored (prop-shape-list-value g shape "ignoredProperties")))
          (push *rdf-type* allowed-preds)
          (dolist (ig ignored) (push ig allowed-preds))
          (dolist (tr (get-triples g :subject focus-node))
            (unless (member (triple-predicate tr) allowed-preds :test #'equal)
              (push (make-violation focus-node (triple-predicate tr) shape
                                    (format nil "predicate not allowed by sh:closed"))
                    violations))))))
    ;; sh:equals (node level) — focus node must be in values of the given path
    (let ((eq-path (prop-shape-value g shape "equals")))
      (when eq-path
        (let ((vals (mapcar #'triple-object (get-triples g :subject focus-node :predicate eq-path))))
          (unless (member focus-node vals :test #'equal)
            (push (make-violation focus-node nil shape
                                  (format nil "focus node not in values of ~A" eq-path))
                  violations)))))
    ;; sh:disjoint (node level)
    (let ((disj-path (prop-shape-value g shape "disjoint")))
      (when disj-path
        (let ((focus-vals (mapcar #'triple-object (get-triples g :subject focus-node)))
              (other-vals (mapcar #'triple-object (get-triples g :subject focus-node :predicate disj-path))))
          (dolist (v focus-vals)
            (when (member v other-vals :test #'equal)
              (push (make-violation focus-node nil shape
                                    (format nil "value ~A overlaps with ~A" v disj-path))
                    violations))))))
    ;; sh:node (node level) — focus node must conform to referenced shape
    (let ((ref-shape (prop-shape-value g shape "node")))
      (when ref-shape
        (unless (check-value-against-subshape g focus-node ref-shape)
          (push (make-violation focus-node nil shape
                                (format nil "does not conform to ~A" ref-shape))
                violations))))
    violations))

;;; ==========================================================================
;;; Main validation entry point
;;; ==========================================================================

(defun shacl-validate (g)
  "Validate graph G against all SHACL shapes defined within it.
Returns a plist with :conforms (boolean) and :results (list of violations)."
  (let ((shapes (find-shapes g))
        (components (find-constraint-components g))
        (all-violations nil))
    (dolist (shape shapes)
      (let ((deact (lit-val (prop-shape-value g shape "deactivated"))))
        (unless (or (eq deact t) (equal deact "true"))
          (let ((targets (shape-targets g shape))
                (prop-shapes (shape-property-shapes g shape))
                (is-prop-shape (has-triple-p g shape *rdf-type* (sh-uri "PropertyShape")))
                (sparql-constraints (mapcar #'triple-object
                                            (get-triples g :subject shape :predicate (sh-uri "sparql")))))
            (dolist (focus targets)
              (let ((node-violations (check-node-constraints g focus shape)))
                (setf all-violations (nconc all-violations node-violations)))
              (when (and is-prop-shape (prop-shape-path g shape))
                (let ((violations (check-property-shape g focus shape shape)))
                  (setf all-violations (nconc all-violations violations))))
              (dolist (ps prop-shapes)
                (let ((ps-deact (lit-val (prop-shape-value g ps "deactivated"))))
                  (unless (or (eq ps-deact t) (equal ps-deact "true"))
                    (let ((violations (check-property-shape g focus ps shape)))
                      (setf all-violations (nconc all-violations violations))))))
              ;; SPARQL constraints
              (dolist (sc sparql-constraints)
                (let* ((path-uri (prop-shape-path g shape))
                       (violations (check-sparql-constraint g focus sc shape :path path-uri)))
                  (setf all-violations (nconc all-violations violations))))
              ;; Custom constraint components on the shape itself
              (dolist (comp components)
                (let ((params (component-parameters g comp))
                      (bindings nil)
                      (all-present t))
                  ;; Check if shape has values for all required parameters
                  (dolist (p params)
                    (let* ((path-uri (second p))
                           (optional-p (third p))
                           (val (when path-uri
                                  (let ((tr (first (get-triples g :subject shape :predicate path-uri))))
                                    (when tr (triple-object tr))))))
                      (if val
                          (let ((local (let ((h (position #\# path-uri)))
                                         (if h (subseq path-uri (1+ h))
                                             (let ((s (position #\/ path-uri :from-end t)))
                                               (if s (subseq path-uri (1+ s)) path-uri))))))
                            (push (cons local val) bindings))
                          (unless optional-p (setf all-present nil)))))
                  (when all-present
                    (let ((v (check-component-constraint g focus comp shape
                                                         :param-bindings bindings)))
                      (setf all-violations (nconc all-violations v))))))
              ;; Custom constraint components on property shapes
              (dolist (ps prop-shapes)
                (dolist (comp components)
                  (let ((params (component-parameters g comp))
                        (bindings nil)
                        (all-present t))
                    (dolist (p params)
                      (let* ((path-uri (second p))
                             (optional-p (third p))
                             (val (when path-uri
                                    (let ((tr (first (get-triples g :subject ps :predicate path-uri))))
                                      (when tr (triple-object tr))))))
                        (if val
                            (let ((local (let ((h (position #\# path-uri)))
                                           (if h (subseq path-uri (1+ h))
                                               (let ((s (position #\/ path-uri :from-end t)))
                                                 (if s (subseq path-uri (1+ s)) path-uri))))))
                              (push (cons local val) bindings))
                            (unless optional-p (setf all-present nil)))))
                    (when all-present
                      (let* ((ps-path (prop-shape-path g ps))
                             (v (check-component-constraint g focus comp ps
                                                            :path ps-path
                                                            :param-bindings bindings)))
                        (setf all-violations (nconc all-violations v))))))))))))
    (list :conforms (null all-violations)
          :results all-violations)))

;;; ==========================================================================
;;; Custom constraint components
;;; ==========================================================================

(defun find-constraint-components (g)
  "Find all SHACL constraint components (resources with sh:parameter)."
  (remove-duplicates
   (mapcar #'triple-subject (get-triples g :predicate (sh-uri "parameter")))
   :test #'equal))

(defun component-parameters (g component)
  "Return list of (param-node path-uri optional-p) for a component."
  (mapcar (lambda (tr)
            (let* ((param (triple-object tr))
                   (path (prop-shape-path g param))
                   (opt (prop-shape-value g param "optional")))
              (list param path (let ((v (lit-val opt))) (or (eq v t) (equal v "true"))))))
          (get-triples g :subject component :predicate (sh-uri "parameter"))))

(defun component-validator (g component kind)
  "Get validator node for component. KIND is :node, :property, or :any."
  (let ((predicates (case kind
                      (:node (list "nodeValidator" "validator"))
                      (:property (list "propertyValidator" "validator"))
                      (t (list "validator" "nodeValidator" "propertyValidator")))))
    (dolist (pred predicates)
      (let ((tr (first (get-triples g :subject component :predicate (sh-uri pred)))))
        (when tr (return-from component-validator (triple-object tr)))))))

(defun check-component-constraint (g focus-node component shape &key path param-bindings)
  "Check a custom constraint component against a focus node."
  (let* ((kind (if path :property :node))
         (validator (component-validator g component kind))
         (violations nil))
    (when validator
      (let ((select-q (lit-val (prop-shape-value g validator "select")))
            (ask-q (lit-val (prop-shape-value g validator "ask")))
            (message (or (lit-val (prop-shape-value g validator "message"))
                         "Custom constraint violation")))
        ;; Collect prefixes from validator
        ;; Check for BIND reassigning pre-bound variables (spec 6.3.3)
        (let ((raw-q (or select-q ask-q "")))
          (when (cl-ppcre:scan "(?i)\\bBIND\\b[^)]*\\bAS\\b\\s*[?$](value|this|PATH)\\b" raw-q)
            (error "SHACL-SPARQL: BIND reassigns pre-bound variable in validator")))
        (let ((prefix-str (collect-shacl-prefixes g validator)))
          (flet ((substitute-params (q)
                   (let* ((result (remove #\Return q))
                          (wp (search "WHERE" (string-upcase result)))
                          (sel-part (if wp (subseq result 0 wp) ""))
                          (whr-part (if wp (subseq result wp) result)))
                     ;; Replace $this: dummy var in SELECT, URI in WHERE
                     (setf sel-part (cl-ppcre:regex-replace-all "\\$this" sel-part "?SHACLthis"))
                     (setf whr-part (cl-ppcre:regex-replace-all
                                     "\\$this" whr-part (format nil "<~A>" focus-node)))
                     (setf result (concatenate 'string sel-part whr-part))
                     ;; Replace $PATH
                     (when path
                       (setf result (cl-ppcre:regex-replace-all
                                     "\\$PATH" result (format nil "<~A>" path))))
                     ;; Replace parameter variables ($name and ?name)
                     (dolist (pb param-bindings)
                       (let* ((name (car pb))
                              (val (cdr pb))
                              (sv (lit-val val))
                              (replacement (if (stringp sv)
                                               (format nil "\"~A\"" sv)
                                               (princ-to-string sv))))
                         (setf result (cl-ppcre:regex-replace-all
                                       (format nil "\\$~A\\b" name) result replacement))
                         (setf result (cl-ppcre:regex-replace-all
                                       (format nil "\\?~A\\b" name) result replacement))))
                     (concatenate 'string prefix-str result))))
            (when select-q
              (let* ((q (substitute-params select-q))
                     (results (handler-case (sparql g q) (error () nil))))
                (when (and results (listp results))
                  (dolist (row results)
                    (let ((row-list (if (listp row) row (list row))))
                      (push (make-violation focus-node
                                            (when path path)
                                            shape message
                                            :value (first row-list))
                            violations))))))
            (when ask-q
              (let* ((values-to-check
                       (if path
                           (mapcar #'triple-object
                                   (get-triples g :subject focus-node :predicate path))
                           (list focus-node)))
                     (q-template (substitute-params ask-q)))
                (dolist (val values-to-check)
                  (let* ((sv (lit-val val))
                         (q (cl-ppcre:regex-replace-all
                             "\\?value" q-template
                             (if (stringp sv)
                                 (format nil "\"~A\"" sv)
                                 (format nil "<~A>" sv))))
                         (result (handler-case (sparql g q) (error () t))))
                    (unless result
                      (push (make-violation focus-node
                                            (when path path)
                                            shape message
                                            :value val)
                            violations))))))))))
    violations))

;;; ==========================================================================
;;; SPARQL-based constraints
;;; ==========================================================================

(defvar *shacl-sparql-forbidden*
  '("MINUS" "VALUES" "SERVICE" "INSERT" "DELETE" "LOAD" "CLEAR" "CREATE" "DROP"
    "COPY" "MOVE" "ADD")
  "SPARQL keywords forbidden in SHACL constraint queries.")

(defun check-shacl-sparql-allowed (query-str)
  "Signal error if SPARQL query uses features forbidden in SHACL constraints."
  (let ((upper (string-upcase query-str)))
    ;; Forbidden keywords
    (dolist (kw *shacl-sparql-forbidden*)
      (when (cl-ppcre:scan (format nil "\\b~A\\b" kw) upper)
        (error "SHACL-SPARQL: ~A not allowed in constraint queries" kw)))
    ;; Nested SELECT with SELECT * or different variables — unsupported
    (let ((first-select (search "SELECT" upper))
          (second-select nil))
      (when first-select
        (setf second-select (search "SELECT" upper :start2 (+ first-select 6))))
      (when second-select
        ;; Allow if inner SELECT uses $THIS, reject SELECT * or other vars
        (let ((after (subseq upper second-select)))
          (unless (cl-ppcre:scan "^SELECT\\s+\\$THIS\\b" after)
            (error "SHACL-SPARQL: unsupported subquery")))))
    ;; BIND reassigning pre-bound variables ($this, $PATH, $shapesGraph, $currentShape)
    (when (cl-ppcre:scan "\\bBIND\\b.*\\bAS\\b.*\\$" upper)
      (error "SHACL-SPARQL: BIND cannot reassign pre-bound variables"))
    ;; Unresolved pre-bound variables other than $this
    (let ((cleaned (cl-ppcre:regex-replace-all "(?i)\\$this\\b|\\$PATH\\b|\\$shapesGraph\\b|\\$currentShape\\b" query-str "")))
      (when (cl-ppcre:scan "\\$[A-Za-z]" cleaned)
        (error "SHACL-SPARQL: unresolved pre-bound variable")))))

(defun collect-shacl-prefixes (g resource)
  "Collect sh:prefixes declarations from resource, following owl:imports."
  (let ((prefix-strs nil)
        (visited nil))
    (labels ((collect-from (node)
               (unless (member node visited :test #'equal)
                 (push node visited)
                 ;; Direct sh:declare on this node
                 (dolist (dt (get-triples g :subject node :predicate (sh-uri "declare")))
                   (let* ((decl (triple-object dt))
                          (pfx (lit-val (prop-shape-value g decl "prefix")))
                          (ns (lit-val (prop-shape-value g decl "namespace"))))
                     (when (and pfx ns)
                       (push (format nil "PREFIX ~A: <~A>" pfx ns) prefix-strs))))
                 ;; Follow owl:imports
                 (dolist (it (get-triples g :subject node :predicate "http://www.w3.org/2002/07/owl#imports"))
                   (collect-from (triple-object it))))))
      (dolist (pf-triple (get-triples g :subject resource :predicate (sh-uri "prefixes")))
        (collect-from (triple-object pf-triple))))
    (if prefix-strs (format nil "~{~A~%~}" (nreverse prefix-strs)) "")))

(defun check-sparql-constraint (g focus-node constraint shape &key path)
  "Execute a sh:sparql constraint against focus-node. Returns list of violations."
  (let* ((select-query (lit-val (prop-shape-value g constraint "select")))
         (ask-query (lit-val (prop-shape-value g constraint "ask")))
         (message (or (lit-val (prop-shape-value g constraint "message")) "SPARQL constraint violation"))
         (prefix-str (let ((p (collect-shacl-prefixes g constraint)))
                       (if (string= p "") (collect-shacl-prefixes g shape) p)))
         (violations nil))
    (when select-query
      (let ((clean-query (remove #\Return select-query)))
        (check-shacl-sparql-allowed clean-query)
      ;; Replace $this: in SELECT list use a dummy var, in WHERE use URI
      (let* ((where-pos (search "WHERE" (string-upcase clean-query)))
             (select-part (if where-pos (subseq clean-query 0 where-pos) ""))
             (where-part (if where-pos (subseq clean-query where-pos) clean-query))
             (fixed-select (cl-ppcre:regex-replace-all "\\$this" select-part "?SHACLthis"))
             (fixed-where (let ((w (cl-ppcre:regex-replace-all
                                   "\\$this"
                                   where-part
                                   (format nil "<~A>" focus-node))))
                           (when path
                             (setf w (cl-ppcre:regex-replace-all "\\$PATH" w (format nil "<~A>" path))))
                           (setf w (cl-ppcre:regex-replace-all
                                    "\\$shapesGraph" w
                                    (format nil "<~A>" (or (graph-name g) "urn:ariadne:default"))))
                           (setf w (cl-ppcre:regex-replace-all
                                    "\\$currentShape" w (format nil "<~A>" shape)))
                           w))
             (fixed-query (concatenate 'string prefix-str fixed-select fixed-where))
             (results (sparql g fixed-query)))
        (when (and results (listp results))
          (dolist (row results)
            (let ((row-list (if (listp row) row (list row))))
              (push (make-violation focus-node
                                    (when (> (length row-list) 1) (second row-list))
                                    shape
                                    (if (stringp message) message (princ-to-string message))
                                    :value (when (> (length row-list) 2) (third row-list)))
                    violations)))))))
    (when ask-query
      (let ((clean-ask (remove #\Return ask-query)))
        (check-shacl-sparql-allowed clean-ask)
        (let* ((query-str (cl-ppcre:regex-replace-all
                           "\\$this"
                           clean-ask
                           (format nil "<~A>" focus-node)))
               (result (sparql g (concatenate 'string prefix-str query-str))))
          (unless result
            (push (make-violation focus-node nil shape
                                  (if (stringp message) message (princ-to-string message)))
                  violations)))))
    violations))
