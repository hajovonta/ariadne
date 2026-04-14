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
(defstruct (alg-group (:constructor make-alg-group (keys pattern))) keys pattern)
(defstruct (alg-agg-join (:constructor make-agg-join (aggregations group-node))) aggregations group-node)
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
    (alg-group    (eval-group-node node graph dataset))
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
  "Property path — delegates to existing path evaluator."
  (let* ((s (alg-path-subject node))
         (o (alg-path-object node))
         (pe (alg-path-path-expr node))
         (pairs (execute-path graph s (first pe) pe o)))
    (mapcar (lambda (pair)
              (let ((mu nil))
                (when (variable-p s) (push (cons s (car pair)) mu))
                (when (variable-p o) (push (cons o (cdr pair)) mu))
                mu))
            pairs)))

(defun eval-group-node (node graph dataset)
  "Group + Aggregation — placeholder, will expand."
  (let* ((omega (eval-algebra (alg-group-pattern node) graph dataset))
         (keys (alg-group-keys node))
         (groups (make-hash-table :test 'equal)))
    (dolist (mu omega)
      (let ((key (mapcar (lambda (k)
                           (if (variable-p k)
                               (lit-val (lookup-binding k mu))
                               (handler-case (safe-eval (subst-vars k mu))
                                 (error () nil))))
                         keys)))
        (push mu (gethash key groups))))
    ;; Return grouped results — aggregation applied by caller
    groups))

(defun eval-agg-join-node (node graph dataset)
  "AggregateJoin — placeholder."
  (declare (ignore node graph dataset))
  (list nil))

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
