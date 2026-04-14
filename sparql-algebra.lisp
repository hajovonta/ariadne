;;;; sparql-algebra.lisp
;;;; SPARQL Algebra — tree representation and recursive evaluator
;;;; Per W3C SPARQL 1.1 Section 18

(in-package #:ariadne)

;;; ============================================================
;;; Algebra node types (Section 18.5)
;;; ============================================================

(defstruct (alg-bgp (:constructor make-bgp (triples))) triples)
(defstruct (alg-join (:constructor make-join (left right))) left right)
(defstruct (alg-left-join (:constructor make-left-join (left right expr))) left right expr)
(defstruct (alg-filter (:constructor make-alg-filter (expr pattern))) expr pattern)
(defstruct (alg-union (:constructor make-alg-union (left right))) left right)
(defstruct (alg-minus (:constructor make-alg-minus (left right))) left right)
(defstruct (alg-extend (:constructor make-alg-extend (pattern var expr))) pattern var expr)
(defstruct (alg-graph (:constructor make-alg-graph (name pattern))) name pattern)
(defstruct (alg-path (:constructor make-alg-path (subject path-expr object))) subject path-expr object)
(defstruct (alg-table (:constructor make-alg-table (vars rows))) vars rows)
(defstruct (alg-project (:constructor make-alg-project (vars pattern))) vars pattern)
(defstruct (alg-distinct (:constructor make-alg-distinct (pattern))) pattern)
(defstruct (alg-slice (:constructor make-alg-slice (pattern start length))) pattern start length)
(defstruct (alg-order (:constructor make-alg-order (conditions pattern))) conditions pattern)
;; Group(exprlist, Ω) → {key→Ω_k} — Section 18.5.1
(defstruct (alg-group (:constructor make-alg-group (keys pattern))) keys pattern)
;; Aggregation(exprlist, func, scalarvals, {key→Ω}) → {key→value} — Section 18.5.1
(defstruct (alg-aggregation (:constructor make-alg-aggregation
                                (exprlist func scalarvals distinct-p group-node)))
  exprlist func scalarvals distinct-p group-node)
;; AggregateJoin(A1,...,An) — Section 18.5.1
(defstruct (alg-agg-join (:constructor make-agg-join (aggregations group-node)))
  aggregations group-node)
(defstruct (alg-exists (:constructor make-alg-exists (pattern negated))) pattern negated)
(defstruct (alg-subquery (:constructor make-alg-subquery (query))) query)

;;; ============================================================
;;; Evaluator (Section 18.6)
;;; ============================================================

(defun eval-algebra (node graph dataset)
  "Evaluate algebra NODE against GRAPH (active graph) in DATASET.
   Returns a list of solution mappings (alists)."
  (etypecase node
    (null         (list nil))  ; empty pattern = Ω0
    (alg-bgp     (eval-bgp-alg (alg-bgp-triples node) graph))
    (alg-join     (eval-join-alg
                   (eval-algebra (alg-join-left node) graph dataset)
                   (eval-algebra (alg-join-right node) graph dataset)))
    (alg-left-join (eval-left-join-alg
                    (eval-algebra (alg-left-join-left node) graph dataset)
                    (eval-algebra (alg-left-join-right node) graph dataset)
                    (alg-left-join-expr node) graph dataset))
    (alg-filter   (eval-filter-node
                   (alg-filter-expr node)
                   (eval-algebra (alg-filter-pattern node) graph dataset)
                   graph dataset))
    (alg-union    (append
                   (eval-algebra (alg-union-left node) graph dataset)
                   (eval-algebra (alg-union-right node) graph dataset)))
    (alg-minus    (eval-minus-node
                   (eval-algebra (alg-minus-left node) graph dataset)
                   (eval-algebra (alg-minus-right node) graph dataset)))
    (alg-extend   (eval-extend-node
                   (eval-algebra (alg-extend-pattern node) graph dataset)
                   (alg-extend-var node) (alg-extend-expr node)))
    (alg-graph    (eval-graph-node node dataset))
    (alg-path     (eval-path-node node graph))
    (alg-table    (eval-table-node node))
    (alg-project  (eval-project-node
                   (eval-algebra (alg-project-pattern node) graph dataset)
                   (alg-project-vars node)))
    (alg-distinct (remove-duplicates
                   (eval-algebra (alg-distinct-pattern node) graph dataset)
                   :test #'equal))
    (alg-slice    (let ((omega (eval-algebra (alg-slice-pattern node) graph dataset)))
                    (eval-slice-node omega (alg-slice-start node) (alg-slice-length node))))
    (alg-order    (eval-order-node
                   (eval-algebra (alg-order-pattern node) graph dataset)
                   (alg-order-conditions node)))
    (alg-group    (error "alg-group should not be evaluated directly; use via alg-agg-join"))
    (alg-aggregation (error "alg-aggregation should not be evaluated directly; use via alg-agg-join"))
    (alg-agg-join (eval-agg-join-node node graph dataset))
    (alg-subquery (eval-algebra (alg-subquery-query node) graph dataset))))

;;; ============================================================
;;; Compatible mappings & merge (Section 18.3)
;;; ============================================================

(defun mappings-compatible-p (mu1 mu2)
  "Two solution mappings are compatible if shared variables have equal values."
  (dolist (b1 mu1 t)
    (let ((b2 (assoc (car b1) mu2)))
      (when (and b2 (cdr b1) (cdr b2))
        (unless (rdf-term-equal-p (cdr b1) (cdr b2))
          (return nil))))))

(defun rdf-term-equal-p (a b)
  (or (equal a b)
      (and a b (equal (lit-val a) (lit-val b)))))

(defun merge-solutions (mu1 mu2)
  "merge(μ1, μ2) = μ1 ∪ μ2 for compatible mappings."
  (let ((result (copy-alist mu1)))
    (dolist (b mu2 result)
      (unless (assoc (car b) result)
        (push b result)))))

;;; ============================================================
;;; Algebra operators
;;; ============================================================

(defun eval-bgp-alg (triples graph)
  "BGP matching — reuses existing pattern matcher."
  (if (null triples)
      (list nil)
      (match-patterns-with-envs graph triples (list nil))))

(defun eval-join-alg (omega1 omega2)
  "Join(Ω1, Ω2)"
  (let ((results nil))
    (dolist (mu1 omega1 (nreverse results))
      (dolist (mu2 omega2)
        (when (mappings-compatible-p mu1 mu2)
          (push (merge-solutions mu1 mu2) results))))))

(defun eval-left-join-alg (omega1 omega2 expr graph dataset)
  "LeftJoin(Ω1, Ω2, expr)"
  (let ((results nil))
    (dolist (mu1 omega1 (nreverse results))
      (let ((matched nil))
        (dolist (mu2 omega2)
          (when (mappings-compatible-p mu1 mu2)
            (let ((merged (merge-solutions mu1 mu2)))
              (when (eval-filter-expr-alg expr merged graph dataset)
                (push merged results)
                (setf matched t)))))
        (unless matched
          (push mu1 results))))))

(defun eval-filter-node (expr omega graph dataset)
  "Filter(expr, Ω)"
  (remove-if-not
   (lambda (mu) (eval-filter-expr-alg expr mu graph dataset))
   omega))

(defun eval-filter-expr-alg (expr mu graph dataset)
  "Evaluate a filter expression against a solution mapping.
   Handles EXISTS/NOT-EXISTS via recursive algebra evaluation."
  (cond
    ((eq expr t) t)
    ((null expr) t)
    ((alg-exists-p expr)
     (let* ((inner (alg-exists-pattern expr))
            (result (eval-algebra inner graph dataset)))
       ;; Substitute current bindings into the inner pattern results
       ;; Per spec: exists(P) is true iff eval(D(G), substitute(P, μ)) is non-empty
       ;; For now, we check if any inner result is compatible with mu
       (let ((found (some (lambda (mu2) (mappings-compatible-p mu mu2)) result)))
       (if (alg-exists-negated expr) (not found) found))))
    (t (handler-case
           (let ((val (safe-eval (subst-vars expr mu))))
             (sparql-ebv val))
         (error () nil)))))

(defun sparql-ebv (val)
  "Effective Boolean Value (Section 17.2.2)."
  (cond
    ((null val) nil)
    ((eq val t) t)
    ((and (numberp val) (zerop val)) nil)
    ((numberp val) t)
    ((and (stringp val) (zerop (length val))) nil)
    ((stringp val) t)
    ((rdf-literal-p val) (sparql-ebv (rdf-literal-value val)))
    (t t)))

(defun eval-minus-node (omega1 omega2)
  "Minus(Ω1, Ω2) — Section 18.5 definition."
  (remove-if
   (lambda (mu1)
     (some (lambda (mu2)
             (and (mappings-compatible-p mu1 mu2)
                  ;; Additional restriction: domains must overlap
                  (let ((dom1 (remove nil (mapcar #'car mu1)))
                        (dom2 (remove nil (mapcar #'car mu2))))
                    (intersection dom1 dom2 :test #'equal))))
           omega2))
   omega1))

(defun eval-extend-node (omega var expr)
  "Extend(Ω, var, expr)"
  (mapcar (lambda (mu)
            (if (assoc var mu)
                mu ; undefined when var in dom(μ)
                (let ((val (handler-case (safe-eval (subst-vars expr mu))
                             (error () nil))))
                  (acons var val mu))))
          omega))

(defun eval-project-node (omega vars)
  "Project(Ω, PV)"
  (mapcar (lambda (mu)
            (remove-if-not (lambda (b) (member (car b) vars :test #'equal)) mu))
          omega))

(defun eval-slice-node (omega start length)
  "Slice(Ω, start, length)"
  (let ((seq (nthcdr (or start 0) omega)))
    (if length
        (subseq seq 0 (min length (length seq)))
        seq)))

(defun eval-order-node (omega conditions)
  "OrderBy(Ω, conditions)"
  (stable-sort (copy-list omega)
               (lambda (a b)
                 (dolist (c conditions nil)
                   (let ((va (eval-order-key c a))
                         (vb (eval-order-key c b)))
                     (cond ((sparql-less-than-p va vb) (return t))
                           ((sparql-less-than-p vb va) (return nil))))))))

(defun eval-order-key (cond mu)
  (if (variable-p cond)
      (lit-val (lookup-binding cond mu))
      (handler-case (safe-eval (subst-vars cond mu)) (error () nil))))

(defun sparql-less-than-p (a b)
  (cond
    ((and (numberp a) (numberp b)) (< a b))
    ((and (stringp a) (stringp b)) (string< a b))
    ((and a b) (string< (princ-to-string a) (princ-to-string b)))
    ((and (null a) b) t)
    (t nil)))

(defun eval-table-node (node)
  "Inline data — VALUES."
  (let ((vars (alg-table-vars node)))
    (mapcar (lambda (row)
              (loop for var in vars for val in row
                    when val collect (cons var val)))
            (alg-table-rows node))))

(defun eval-graph-node (node dataset)
  "GRAPH evaluation (Section 18.6)."
  (let ((name (alg-graph-name node))
        (pattern (alg-graph-pattern node)))
    (if (variable-p name)
        (let ((results nil))
          (dolist (ng (dataset-named-graphs dataset) results)
            (let ((sub (eval-algebra pattern (cdr ng) dataset)))
              (dolist (mu sub)
                (push (acons name (car ng) mu) results)))))
        (let ((g (dataset-get-graph dataset name)))
          (if g (eval-algebra pattern g dataset) nil)))))

(defun eval-path-node (node graph)
  "Property path — delegates to existing path evaluator.
   Handles unbound start/target by iterating graph nodes."
  (let* ((s (alg-path-subject node))
         (o (alg-path-object node))
         (pe (alg-path-path-expr node))
         (op (first pe))
         (bound-s (not (variable-p s)))
         (bound-o (and o (not (variable-p o)))))
    (cond
      ;; Both bound or start bound: call directly
      (bound-s
       (let ((pairs (execute-path graph s op pe (if bound-o o nil)))
             (results nil)
             ;; Sequence/alt paths preserve multiplicity; *, ?, + deduplicate
             (dedup (member (symbol-name op) '("*" "?" "+" "ZEROORONE") :test #'string-equal)))
         (dolist (pair pairs results)
           (let ((mu nil))
             (when (variable-p s) (push (cons s (car pair)) mu))
             (when (and o (variable-p o)) (push (cons o (cdr pair)) mu))
             (if dedup
                 (pushnew mu results :test #'equal)
                 (push mu results))))))
      ;; Unbound start, bound target: try target + all nodes
      (bound-o
       (let ((starts (cons o (all-nodes graph)))
             (results nil))
         (dolist (start (remove-duplicates starts :test #'equal))
           (let ((pairs (execute-path graph start op pe o)))
             (dolist (pair pairs)
               (let ((mu nil))
                 (when (variable-p s) (push (cons s (car pair)) mu))
                 (when (variable-p o) (push (cons o (cdr pair)) mu))
                 (pushnew mu results :test #'equal)))))
         results))
      ;; Both unbound: all nodes as starts
      (t
       (let ((results nil))
         (dolist (start (all-nodes graph))
           (let ((pairs (execute-path graph start op pe nil)))
             (dolist (pair pairs)
               (let ((mu nil))
                 (when (variable-p s) (push (cons s (car pair)) mu))
                 (when (variable-p o) (push (cons o (cdr pair)) mu))
                 (pushnew mu results :test #'equal)))))
         results)))))

(defun eval-group-node (node graph dataset)
  "Group(exprlist, Ω) → hash {key → list-of-solutions}.
   Section 18.5.1: Group evaluates exprlist against each μ to produce keys."
  (let* ((omega (eval-algebra (alg-group-pattern node) graph dataset))
         (keys (alg-group-keys node))
         (groups (make-hash-table :test 'equal)))
    (dolist (mu omega)
      (let ((key (mapcar (lambda (k)
                           (if (variable-p k)
                               (lookup-binding k mu)
                               (handler-case (safe-eval (subst-vars k mu))
                                 (error () :error))))
                         keys)))
        (push mu (gethash key groups))))
    ;; Per spec: implicit grouping (constant key like 1) always produces
    ;; at least one group, even if Ω is empty
    (when (and (null omega)
               (every (lambda (k) (not (variable-p k))) keys))
      (let ((key (mapcar (lambda (k) (handler-case (safe-eval k) (error () k))) keys)))
        (setf (gethash key groups) nil)))
    groups))

(defun eval-aggregation (agg-node graph dataset)
  "Aggregation(exprlist, func, scalarvals, {key→Ω}) → hash {key → scalar}.
   Section 18.5.1."
  (let* ((grouped (eval-group-node (alg-aggregation-group-node agg-node) graph dataset))
         (exprlist (alg-aggregation-exprlist agg-node))
         (func (alg-aggregation-func agg-node))
         (scalarvals (alg-aggregation-scalarvals agg-node))
         (distinct-p (alg-aggregation-distinct-p agg-node))
         (result (make-hash-table :test 'equal)))
    (maphash
     (lambda (key omega-k)
       ;; M(Ω) = { ListEval(exprlist, μ) | μ in Ω }
       (let ((m (mapcar (lambda (mu)
                          (mapcar (lambda (e)
                                    (if (and (symbolp e) (sym-name-equal e "*"))
                                        mu  ; COUNT(*) special case
                                        (handler-case
                                            (let ((v (if (variable-p e)
                                                         (lookup-binding e mu)
                                                         (safe-eval (subst-vars e mu)))))
                                              v)
                                          (error () :error))))
                                  exprlist))
                        omega-k)))
         ;; Apply DISTINCT if specified
         (when distinct-p
           (setf m (remove-duplicates m :test #'equal)))
         ;; F(Ω) = func(M(Ω), scalarvals)
         (setf (gethash key result)
               (apply-set-function func m scalarvals (length omega-k)))))
     grouped)
    result))

(defun apply-set-function (func m scalarvals group-size)
  "Apply a SPARQL set function. Section 18.5.1.1-8.
   M is a list of value-lists (from ListEval). Flatten first."
  (let* ((flat (loop for row in m append row))
         ;; Remove errors
         (clean (remove :error flat))
         (fname (if (symbolp func) (symbol-name func) func)))
    (cond
      ;; COUNT — Section 18.5.1.2
      ((string-equal fname "COUNT")
       ;; Special case: COUNT(*) uses group size
       (if (and (first (first m)) (not (atom (first (first m)))))
           group-size  ; COUNT(*) — m contains full solution mappings
           (length clean)))
      ;; SUM — Section 18.5.1.3
      ((string-equal fname "SUM")
       (let ((nums (remove nil (mapcar (lambda (v) (let ((n (lit-val v))) (when (numberp n) n))) clean))))
         (if nums (reduce #'+ nums) 0)))
      ;; AVG — Section 18.5.1.4
      ((string-equal fname "AVG")
       (let ((nums (remove nil (mapcar (lambda (v) (let ((n (lit-val v))) (when (numberp n) n))) clean))))
         (if nums (/ (reduce #'+ nums) (length nums)) 0)))
      ;; MIN — Section 18.5.1.5
      ((string-equal fname "MIN")
       (let ((nums (remove nil (mapcar (lambda (v) (let ((n (lit-val v))) (when (numberp n) n))) clean))))
         (when nums (reduce #'min nums))))
      ;; MAX — Section 18.5.1.6
      ((string-equal fname "MAX")
       (let ((nums (remove nil (mapcar (lambda (v) (let ((n (lit-val v))) (when (numberp n) n))) clean))))
         (when nums (reduce #'max nums))))
      ;; GROUP_CONCAT — Section 18.5.1.7
      ((string-equal fname "GROUP_CONCAT")
       (let ((sep (or (cdr (assoc "separator" scalarvals :test #'string-equal)) " "))
             (strs (mapcar (lambda (v) (princ-to-string (lit-val v))) clean)))
         (format nil "~{~A~}" (loop for (s . rest) on strs collect s when rest collect sep))))
      ;; SAMPLE — Section 18.5.1.8
      ((string-equal fname "SAMPLE")
       (first clean))
      (t (error "Unknown aggregate: ~A" func)))))

(defun eval-agg-join-node (node graph dataset)
  "AggregateJoin(A1,...,An) — Section 18.5.1.
   Combines multiple aggregation results into solution mappings."
  (let* ((aggs (alg-agg-join-aggregations node))
         (group-node (alg-agg-join-group-node node))
         (grouped (eval-group-node group-node graph dataset))
         (group-keys (alg-group-keys group-node))
         (results nil))
    (if (null aggs)
        ;; GROUP BY without aggregates — one row per group key
        (maphash
         (lambda (key _)
           (declare (ignore _))
           (let ((mu nil))
             (loop for k in group-keys for v in key do
               (when (variable-p k) (push (cons k v) mu)))
             (push mu results)))
         grouped)
        ;; With aggregates — evaluate each, combine
        (let ((agg-results (mapcar (lambda (a)
                                     (cons (car a) (eval-aggregation (cdr a) graph dataset)))
                                   aggs)))
          ;; Gather all keys from the group
          (maphash
           (lambda (key _)
             (declare (ignore _))
             (let ((mu nil))
               ;; Add group key bindings
               (loop for k in group-keys for v in key do
                 (when (variable-p k) (push (cons k v) mu)))
               ;; Add aggregate bindings
               (dolist (ar agg-results)
                 (let ((val (gethash key (cdr ar))))
                   (push (cons (car ar) val) mu)))
               (push mu results)))
           grouped)))
    results))

;;; ============================================================
;;; Dataset abstraction
;;; ============================================================

(defun make-dataset (default-graph &optional named-graphs)
  "Create a dataset: (default-graph . ((name . graph) ...))"
  (cons default-graph named-graphs))

(defun dataset-default-graph (ds) (car ds))
(defun dataset-named-graphs (ds) (cdr ds))
(defun dataset-get-graph (ds name)
  (cdr (assoc name (cdr ds) :test #'equal)))

;;; ============================================================
;;; Translation: parsed query → algebra tree (Section 18.2.2)
;;; ============================================================

(defun translate-query (parsed-expr)
  "Translate a parsed SPARQL query into an algebra tree.
   Returns (values algebra-node query-form vars)."
  (let* ((form (first parsed-expr))  ; SELECT, ASK, CONSTRUCT, etc.
         (form-name (symbol-name form)))
    (cond
      ((or (string-equal form-name "SELECT")
           (string-equal form-name "SELECT-DISTINCT"))
       (translate-select parsed-expr))
      ((string-equal form-name "ASK")
       (translate-ask parsed-expr))
      ((string-equal form-name "CONSTRUCT")
       (translate-construct parsed-expr))
      (t (error "Unknown query form: ~A" form)))))

(defun translate-select (expr)
  "Translate SELECT query. Returns (values algebra :select vars)."
  (let* ((form (first expr))
         (vars (second expr))
         (body (cddr expr))
         (distinct-p (sym-name-equal form "SELECT-DISTINCT"))
         ;; Extract clauses
         (where-clause nil)
         (group-by nil)
         (group-exprs nil)
         (having nil)
         (order-by nil)
         (limit-n nil)
         (offset-n nil)
         (projections nil)
         (values-clause nil))
    (dolist (clause body)
      (let ((tag (and (consp clause) (first clause))))
        (when tag
          (cond
            ((sym-name-equal tag "WHERE") (setf where-clause (rest clause)))
            ((sym-name-equal tag "PROJECT")
             (push (list (second clause) (third clause)) projections))
            ((sym-name-equal tag "GROUP-BY") (setf group-by (second clause)))
            ((sym-name-equal tag "GROUP-BY-MULTI") (setf group-by (second clause)))
            ((sym-name-equal tag "GROUP-BY-EXPR")
             (setf group-by (second clause))
             (push (list (second clause) (third clause)) group-exprs))
            ((sym-name-equal tag "HAVING")
             (setf having (second clause)))
            ((sym-name-equal tag "ORDER-BY") (setf order-by (second clause)))
            ((sym-name-equal tag "LIMIT") (setf limit-n (second clause)))
            ((sym-name-equal tag "OFFSET") (setf offset-n (second clause)))
            ((sym-name-equal tag "VALUES") (setf values-clause (rest clause)))
            ;; OPTIONAL, UNION, MINUS etc. are inside WHERE
            ;; FILTER is top-level — append to WHERE for translate-group
            ((sym-name-equal tag "FILTER")
             (dolist (f (rest clause))
               (setf where-clause (append where-clause (list (list 'filter f))))))
            ))))
    ;; Step 1: Translate the WHERE group graph pattern
    ;; All group elements (triples, OPTIONAL, MINUS, UNION, etc.) are in where-clause in parse order
    (let ((pattern (translate-group where-clause)))
      ;; Step 2: GROUP BY expressions — add Extend nodes
      (dolist (ge (nreverse group-exprs))
        (setf pattern (make-alg-extend pattern (first ge) (second ge))))
      ;; Step 3: Implicit grouping if aggregates used without GROUP BY
      (when (and (null group-by) projections
                 (some (lambda (p) (aggregate-expr-p (second p))) projections))
        (setf group-by (list 1)))
      ;; Step 4: GROUP BY + Aggregation + AggregateJoin (Section 18.2.4.1)
      (when group-by
        (let* ((group-keys (if (listp group-by) group-by (list group-by)))
               (group-node (make-alg-group group-keys pattern))
               (agg-pairs nil)
               (agg-counter 0)
               (extend-pairs nil))
          ;; For each aggregate in projections, create an Aggregation node
          (dolist (proj projections)
            (let ((alias (first proj))
                  (expr (second proj)))
              (when (aggregate-expr-p expr)
                (incf agg-counter)
                (let* ((fn-name (symbol-name (car expr)))
                       (distinct-p (search "DISTINCT" fn-name))
                       (base-fn (if distinct-p
                                    (subseq fn-name 0 (search "-DISTINCT" fn-name))
                                    fn-name))
                       (agg-var (cadr expr))
                       (sep (caddr expr))
                       (scalarvals (when sep (list (cons "separator"
                                                        (if (rdf-literal-p sep)
                                                            (rdf-literal-value sep)
                                                            sep)))))
                       (exprlist (if (and (symbolp agg-var) (sym-name-equal agg-var "*"))
                                     (list '*)
                                     (list agg-var)))
                       (agg (make-alg-aggregation exprlist base-fn scalarvals distinct-p group-node)))
                  (push (cons alias agg) agg-pairs)))))
          ;; Non-aggregate projections that reference group vars → Extend later
          (dolist (proj projections)
            (unless (aggregate-expr-p (second proj))
              (push proj extend-pairs)))
          ;; Also scan HAVING for aggregates and create Aggregation nodes
          (when having
            (let ((having-list (if (and (consp having) (consp (first having))) having (list having)))
                  (new-having nil))
              (dolist (h having-list)
                (multiple-value-bind (new-h new-pairs new-c)
                    (replace-aggregates-with-vars h group-node agg-pairs agg-counter)
                  (setf agg-pairs new-pairs agg-counter new-c)
                  (push new-h new-having)))
              (setf having (nreverse new-having))))
          (if agg-pairs
              ;; Build AggregateJoin
              (progn
                (setf pattern (make-agg-join (nreverse agg-pairs) group-node))
                ;; Apply Extend for non-aggregate projections
                (dolist (ep (nreverse extend-pairs))
                  (setf pattern (make-alg-extend pattern (first ep) (second ep)))))
              ;; GROUP BY without aggregates — just group and take one per group
              (setf pattern (make-agg-join nil group-node)))))
      ;; Step 5: HAVING
      (when having
        (dolist (h (if (and (consp having) (consp (first having))) having (list having)))
          (setf pattern (make-alg-filter h pattern))))
      ;; Step 6: Post-query VALUES
      (when values-clause
        (let ((table (make-alg-table (first values-clause) (second values-clause))))
          (setf pattern (make-join pattern table))))
      ;; Step 7: SELECT expressions (Extend for non-grouped computed columns)
      (when (null group-by)
        (dolist (proj (nreverse projections))
          (unless (aggregate-expr-p (second proj))
            (setf pattern (make-alg-extend pattern (first proj) (second proj))))))
      ;; Step 8: Solution modifiers
      (when order-by
        (setf pattern (make-alg-order (if (listp order-by) order-by (list order-by)) pattern)))
      ;; Projection
      (let ((pv (if (or (eq vars '*) (equal vars '("*")))
                     nil  ; SELECT * — no projection
                     vars)))
        (when pv
          (setf pattern (make-alg-project pv pattern))))
      ;; DISTINCT
      (when distinct-p
        (setf pattern (make-alg-distinct pattern)))
      ;; OFFSET/LIMIT
      (when (or offset-n limit-n)
        (setf pattern (make-alg-slice pattern
                                      (when offset-n (if (numberp offset-n) offset-n (parse-integer (princ-to-string offset-n))))
                                      (when limit-n (if (numberp limit-n) limit-n (parse-integer (princ-to-string limit-n)))))))
      (values pattern :select vars))))

(defun translate-ask (expr)
  "Translate ASK query."
  (let* ((body (rest expr))
         (where-clause nil)
         (filters nil))
    (dolist (clause body)
      (let ((tag (and (consp clause) (first clause))))
        (cond
          ((and tag (sym-name-equal tag "WHERE")) (setf where-clause (rest clause)))
          ((and tag (sym-name-equal tag "FILTER")) (setf filters (rest clause)))
          ((and tag (sym-name-equal tag "BIND")) nil) ; handled in WHERE
          )))
    (let ((pattern (translate-group (append where-clause
                                           (mapcar (lambda (f) (list 'filter f)) filters)))))
      (values pattern :ask nil))))

(defun translate-construct (expr)
  "Translate CONSTRUCT query."
  (let ((template (second expr))
        (where-clause nil))
    (dolist (clause (cddr expr))
      (when (and (consp clause) (sym-name-equal (first clause) "WHERE"))
        (setf where-clause (rest clause))))
    (let ((pattern (translate-group where-clause)))
      (values pattern :construct template))))

;;; ============================================================
;;; Translate Group Graph Pattern (Section 18.2.2.6)
;;; ============================================================

(defun translate-group (elements)
  "Translate a group graph pattern into an algebra node.
   Implements the algorithm from Section 18.2.2.6."
  (when (null elements) (return-from translate-group nil))
  (let ((filters nil)
        (g nil))  ; starts as empty pattern (nil = Ω0)
    ;; First pass: collect FILTERs (they apply to the whole group)
    ;; and translate EXISTS/NOT-EXISTS within them
    (let ((non-filter-elements nil))
      (dolist (e elements)
        (if (and (consp e) (symbolp (car e)) (sym-name-equal (car e) "FILTER"))
            (push (translate-filter-expr (second e)) filters)
            (push e non-filter-elements)))
      (setf non-filter-elements (nreverse non-filter-elements))
      ;; Second pass: process each element in order
      ;; Adjacent triple patterns are collected into a single BGP (Section 18.2.2.5)
      (let ((pending-triples nil))
        (flet ((flush-bgp ()
                 (when pending-triples
                   (setf g (make-join g (make-bgp (nreverse pending-triples))))
                   (setf pending-triples nil))))
      (dolist (e non-filter-elements)
        (cond
          ;; OPTIONAL {P}
          ((and (consp e) (symbolp (car e)) (sym-name-equal (car e) "OPTIONAL"))
           (flush-bgp)
           (let* ((inner (translate-group (rest e)))
                  ;; Check if inner is Filter(F, A) — extract F for LeftJoin
                  (filter-expr (when (alg-filter-p inner) (alg-filter-expr inner)))
                  (inner-pattern (if (alg-filter-p inner) (alg-filter-pattern inner) inner)))
             (setf g (make-left-join g inner-pattern (or filter-expr t)))))
          ;; MINUS {P}
          ((and (consp e) (symbolp (car e)) (sym-name-equal (car e) "MINUS"))
           (flush-bgp)
           (setf g (make-alg-minus g (translate-group (rest e)))))
          ;; BIND (expr AS var)
          ((and (consp e) (symbolp (car e))
                (or (sym-name-equal (car e) "BIND")
                    (sym-name-equal (car e) "INLINE-BIND")))
           (flush-bgp)
           (setf g (make-alg-extend g (second e) (third e))))
          ;; VALUES
          ((and (consp e) (symbolp (car e)) (sym-name-equal (car e) "VALUES"))
           (flush-bgp)
           (let ((table (make-alg-table (second e) (third e))))
             (setf g (make-join g table))))
          ;; UNION
          ((and (consp e) (symbolp (car e)) (sym-name-equal (car e) "UNION"))
           (flush-bgp)
           (let ((branches (rest e)))
             (let ((u (translate-group (first branches))))
               (dolist (b (rest branches))
                 (setf u (make-alg-union u (translate-group b))))
               (setf g (make-join g u)))))
          ;; GRAPH
          ((and (consp e) (symbolp (car e)) (sym-name-equal (car e) "GRAPH"))
           (flush-bgp)
           (let ((name (second e))
                 (inner (translate-group (cddr e))))
             (setf g (make-join g (make-alg-graph name inner)))))
          ;; NOT-EXISTS (as pattern, not in filter)
          ((and (consp e) (symbolp (car e)) (sym-name-equal (car e) "NOT-EXISTS"))
           (flush-bgp)
           (push (make-alg-exists (translate-group (rest e)) t) filters))
          ;; EXISTS (as pattern, not in filter)
          ((and (consp e) (symbolp (car e)) (sym-name-equal (car e) "EXISTS"))
           (flush-bgp)
           (push (make-alg-exists (translate-group (rest e)) nil) filters))
          ;; SUBQUERY
          ((and (consp e) (symbolp (car e)) (sym-name-equal (car e) "SUBQUERY"))
           (flush-bgp)
           (multiple-value-bind (sub-alg) (translate-query (second e))
             (setf g (make-join g sub-alg))))
          ;; Property path pattern: (s (path-op ...) o)
          ((and (consp e) (= 3 (length e))
                (consp (second e)) (symbolp (first (second e))))
           (flush-bgp)
           (setf g (make-join g (make-alg-path (first e) (second e) (third e)))))
          ;; Triple pattern — collect into pending BGP
          ((and (consp e) (= 3 (length e)))
           (push e pending-triples))
          ;; Unknown — skip
          (t nil)))
      ;; Flush any remaining pending triples
      (flush-bgp))))
    ;; Apply collected filters to the whole group
    (dolist (f (nreverse filters))
      (setf g (make-alg-filter f g)))
    ;; Simplification: Join(nil, A) → A
    (simplify-algebra g)))

(defun simplify-algebra (node)
  "Simplification step (Section 18.2.2.8): remove joins with empty BGP."
  (cond
    ((null node) nil)
    ((alg-join-p node)
     (let ((l (alg-join-left node))
           (r (alg-join-right node)))
       (cond
         ((null l) r)
         ((null r) l)
         ((and (alg-bgp-p l) (null (alg-bgp-triples l))) r)
         ((and (alg-bgp-p r) (null (alg-bgp-triples r))) l)
         (t node))))
    (t node)))

(defun translate-filter-expr (expr)
  "Translate filter expression, converting EXISTS/NOT-EXISTS patterns."
  (cond
    ((atom expr) expr)
    ((and (symbolp (car expr)) (sym-name-equal (car expr) "NOT-EXISTS"))
     (make-alg-exists (translate-group (rest expr)) t))
    ((and (symbolp (car expr)) (sym-name-equal (car expr) "EXISTS"))
     (make-alg-exists (translate-group (rest expr)) nil))
    ;; Recurse into compound expressions (AND, OR, NOT, etc.)
    ((and (symbolp (car expr))
          (member (symbol-name (car expr)) '("AND" "OR" "NOT") :test #'string-equal))
     (cons (car expr) (mapcar #'translate-filter-expr (rest expr))))
    (t expr)))

(defun aggregate-expr-p (expr)
  "Check if expression is an aggregate function call."
  (and (consp expr) (symbolp (car expr))
       (member (symbol-name (car expr))
               '("COUNT" "SUM" "AVG" "MIN" "MAX" "GROUP_CONCAT" "SAMPLE"
                 "COUNT-DISTINCT" "SUM-DISTINCT" "AVG-DISTINCT" "MIN-DISTINCT"
                 "MAX-DISTINCT" "GROUP_CONCAT-DISTINCT" "SAMPLE-DISTINCT"
                 "GROUP-CONCAT" "GROUP-CONCAT-DISTINCT")
               :test #'string-equal)))

(defun replace-aggregates-with-vars (expr group-node agg-pairs counter)
  "Walk expr, replace aggregate calls with temp variables, push Aggregation nodes.
   Returns (values new-expr new-agg-pairs new-counter)."
  (cond
    ((atom expr) (values expr agg-pairs counter))
    ((aggregate-expr-p expr)
     (incf counter)
     (let* ((temp-var (intern (format nil "?_HAVING_AGG~A" counter)))
            (fn-name (symbol-name (car expr)))
            (distinct-p (search "DISTINCT" fn-name))
            (base-fn (if distinct-p
                         (subseq fn-name 0 (search "-DISTINCT" fn-name))
                         fn-name))
            (agg-var (cadr expr))
            (sep (caddr expr))
            (scalarvals (when sep (list (cons "separator"
                                              (if (rdf-literal-p sep) (rdf-literal-value sep) sep)))))
            (exprlist (if (and (symbolp agg-var) (sym-name-equal agg-var "*"))
                          (list '*) (list agg-var)))
            (agg (make-alg-aggregation exprlist base-fn scalarvals distinct-p group-node)))
       (push (cons temp-var agg) agg-pairs)
       (values temp-var agg-pairs counter)))
    (t (multiple-value-bind (new-car ap1 c1)
           (replace-aggregates-with-vars (car expr) group-node agg-pairs counter)
         (multiple-value-bind (new-cdr ap2 c2)
             (replace-aggregates-with-vars (cdr expr) group-node ap1 c1)
           (values (cons new-car new-cdr) ap2 c2))))))

(defun replace-agg-in-expr (expr group-node agg-pairs counter)
  "Convenience wrapper — returns only the new expression, mutates agg-pairs via setf."
  (multiple-value-bind (new-expr new-pairs new-counter)
      (replace-aggregates-with-vars expr group-node agg-pairs counter)
    ;; We need to propagate the side effects back. Use a trick:
    ;; Return the new expr. Caller must capture new pairs/counter.
    ;; Actually, since we can't mutate the caller's bindings, let's just
    ;; return all three and have the caller destructure.
    (values new-expr new-pairs new-counter)))

;;; ============================================================
;;; New entry point: sparql-via-algebra
;;; ============================================================

(defun sparql-via-algebra (graph query-string)
  "Execute a SPARQL query using the algebra evaluator."
  (let* ((parsed (parse-sparql query-string))
         (ds (make-dataset graph)))
    (multiple-value-bind (algebra form vars) (translate-query parsed)
      (let ((results (eval-algebra algebra graph ds)))
        (case form
          (:ask (not (null results)))
          (:select
           ;; Project to result rows
           (if (or (eq vars '*) (equal vars '("*")))
               (mapcar (lambda (mu)
                         (mapcar #'cdr (remove-if (lambda (b) (null (cdr b))) mu)))
                       results)
               (mapcar (lambda (mu)
                         (mapcar (lambda (v) (lookup-binding v mu)) vars))
                       results)))
          (:construct
           ;; Template instantiation
           (let ((tmpl (if (and vars (consp (first vars)) (= 3 (length (first vars))))
                           vars (list vars))))
             (let ((triples nil))
               (dolist (mu results (remove-duplicates (nreverse triples) :test #'equal))
                 (dolist (tp tmpl)
                   (let ((s (subst-vars (first tp) mu))
                         (p (subst-vars (second tp) mu))
                         (o (subst-vars (third tp) mu)))
                     (when (and s p o (not (variable-p s)) (not (variable-p p)) (not (variable-p o)))
                       (push (list s p o) triples)))))))))))))
