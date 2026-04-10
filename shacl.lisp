;;;; shacl.lisp
;;;; SHACL (Shapes Constraint Language) validation — W3C Recommendation

(in-package #:ariadne)

(defparameter *sh* "http://www.w3.org/ns/shacl#")
(defparameter *rdf-type* "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
(defparameter *xsd* "http://www.w3.org/2001/XMLSchema#")

(defun sh-uri (name) (concatenate 'string *sh* name))

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
     (let ((inverse (first (get-triples g :subject path :predicate (sh-uri "inversePath"))))
           (alt-list (first (get-triples g :subject path :predicate (sh-uri "alternativePath"))))
           (zero-more (first (get-triples g :subject path :predicate (sh-uri "zeroOrMorePath"))))
           (one-more (first (get-triples g :subject path :predicate (sh-uri "oneOrMorePath"))))
           (zero-one (first (get-triples g :subject path :predicate (sh-uri "zeroOrOnePath")))))
       (cond
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
         ;; Sequence path (RDF list — path is a list head)
         ((get-triples g :subject path :predicate "http://www.w3.org/1999/02/22-rdf-syntax-ns#first")
          (let ((steps (rdf-list-to-list g path)))
            (sequence-path-values g (list focus-node) steps)))
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
      (let ((allowed (prop-shape-list-value g prop-shape "in")))
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
                    violations))))))
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
              (when (and (or (numberp val) (stringp val))
                         (or (numberp ov) (stringp ov)))
                (let ((fail (cond ((and (numberp val) (numberp ov)) (not (< val ov)))
                                  ((and (stringp val) (stringp ov)) (not (string< val ov)))
                                  (t nil))))
                  (when fail
                    (push (make-violation focus-node path shape
                                          (format nil "~A not < ~A" val ov) :value val)
                          violations)))))))))
    ;; sh:lessThanOrEquals
    (let ((lte-path (prop-shape-value g prop-shape "lessThanOrEquals")))
      (when lte-path
        (let ((other-vals (mapcar #'triple-object (get-triples g :subject focus-node :predicate lte-path))))
          (dolist (val values)
            (dolist (ov other-vals)
              (when (and (or (numberp val) (stringp val))
                         (or (numberp ov) (stringp ov)))
                (let ((fail (cond ((and (numberp val) (numberp ov)) (not (<= val ov)))
                                  ((and (stringp val) (stringp ov)) (not (string<= val ov)))
                                  (t nil))))
                  (when fail
                    (push (make-violation focus-node path shape
                                          (format nil "~A not <= ~A" val ov) :value val)
                          violations)))))))))
    ;; sh:uniqueLang
    (let ((ul (prop-shape-value g prop-shape "uniqueLang")))
      (when (or (eq ul t) (equal ul "true"))
        (let ((seen nil))
          (dolist (val values)
            (when (stringp val)
              (let ((at (position #\@ val :from-end t)))
                (when at
                  (let ((lang (subseq val (1+ at))))
                    (if (member lang seen :test #'string-equal)
                        (push (make-violation focus-node path shape
                                              (format nil "duplicate language: ~A" lang)
                                              :value val)
                              violations)
                        (push lang seen))))))))))
    ;; sh:languageIn
    (let ((lang-list (prop-shape-list-value g prop-shape "languageIn")))
      (when lang-list
        (dolist (val values)
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
                      (push (make-violation focus-node path shape
                                            (format nil "language ~A not in ~S" lang lang-list)
                                            :value val)
                            violations)))
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
        (maxe (prop-shape-value g sub-shape "maxExclusive"))
        (cls (prop-shape-value g sub-shape "class"))
        (hv (prop-shape-value g sub-shape "hasValue"))
        (in-list (prop-shape-list-value g sub-shape "in"))
        (not-shape (prop-shape-value g sub-shape "not"))
        (and-shapes (prop-shape-list-value g sub-shape "and"))
        (or-shapes (prop-shape-list-value g sub-shape "or"))
        (xone-shapes (prop-shape-list-value g sub-shape "xone"))
        (prop-shapes (shape-property-shapes g sub-shape)))
    (and (or (null dt) (value-matches-datatype-p val dt))
         (or (null pat) (not (stringp val)) (cl-ppcre:scan pat val))
         (or (null nk) (value-matches-node-kind-p val nk))
         (or (null mini) (not (numberp val)) (not (numberp mini)) (>= val mini))
         (or (null maxi) (not (numberp val)) (not (numberp maxi)) (<= val maxi))
         (or (null mine) (not (numberp val)) (not (numberp mine)) (> val mine))
         (or (null maxe) (not (numberp val)) (not (numberp maxe)) (< val maxe))
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
                    prop-shapes)))))

;;; ==========================================================================
;;; Node-level constraint checking
;;; ==========================================================================

(defun check-node-constraints (g focus-node shape)
  "Check constraints placed directly on a NodeShape against the focus node."
  (let ((violations nil)
        (val focus-node))
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
    (let ((pat (prop-shape-value g shape "pattern")))
      (when (and pat (stringp val) (not (cl-ppcre:scan pat val)))
        (push (make-violation focus-node nil shape
                              (format nil "does not match pattern ~A" pat))
              violations)))
    ;; sh:minInclusive/maxInclusive/minExclusive/maxExclusive
    (when (numberp val)
      (let ((mini (prop-shape-value g shape "minInclusive")))
        (when (and mini (numberp mini) (< val mini))
          (push (make-violation focus-node nil shape
                                (format nil "value < minInclusive ~A" mini))
                violations)))
      (let ((maxi (prop-shape-value g shape "maxInclusive")))
        (when (and maxi (numberp maxi) (> val maxi))
          (push (make-violation focus-node nil shape
                                (format nil "value > maxInclusive ~A" maxi))
                violations)))
      (let ((mine (prop-shape-value g shape "minExclusive")))
        (when (and mine (numberp mine) (<= val mine))
          (push (make-violation focus-node nil shape
                                (format nil "value <= minExclusive ~A" mine))
                violations)))
      (let ((maxe (prop-shape-value g shape "maxExclusive")))
        (when (and maxe (numberp maxe) (>= val maxe))
          (push (make-violation focus-node nil shape
                                (format nil "value >= maxExclusive ~A" maxe))
                violations))))
    ;; sh:minLength/maxLength
    (when (stringp val)
      (let ((min-l (prop-shape-value g shape "minLength")))
        (when min-l
          (let ((n (if (numberp min-l) min-l (parse-integer (princ-to-string min-l) :junk-allowed t))))
            (when (and n (< (length val) n))
              (push (make-violation focus-node nil shape
                                    (format nil "length < minLength ~A" n))
                    violations)))))
      (let ((max-l (prop-shape-value g shape "maxLength")))
        (when max-l
          (let ((n (if (numberp max-l) max-l (parse-integer (princ-to-string max-l) :junk-allowed t))))
            (when (and n (> (length val) n))
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
    ;; sh:closed
    (let ((closed (prop-shape-value g shape "closed")))
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
        (all-violations nil))
    (dolist (shape shapes)
      (let ((deact (prop-shape-value g shape "deactivated")))
        (unless (or (eq deact t) (equal deact "true"))
          (let ((targets (shape-targets g shape))
                (prop-shapes (shape-property-shapes g shape))
                (is-prop-shape (has-triple-p g shape *rdf-type* (sh-uri "PropertyShape"))))
            (dolist (focus targets)
              (let ((node-violations (check-node-constraints g focus shape)))
                (setf all-violations (nconc all-violations node-violations)))
              ;; If shape is itself a PropertyShape with sh:path, validate it directly
              (when (and is-prop-shape (prop-shape-path g shape))
                (let ((violations (check-property-shape g focus shape shape)))
                  (setf all-violations (nconc all-violations violations))))
              (dolist (ps prop-shapes)
                (let ((ps-deact (prop-shape-value g ps "deactivated")))
                  (unless (or (eq ps-deact t) (equal ps-deact "true"))
                    (let ((violations (check-property-shape g focus ps shape)))
                      (setf all-violations (nconc all-violations violations)))))))))))
    (list :conforms (null all-violations)
          :results all-violations)))
