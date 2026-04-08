;;;; query.lisp
;;;; SPARQL-like query DSL

(in-package #:ariadne)

;;; ==========================================================================
;;; Query Engine
;;; ==========================================================================

(defun sym-name-equal (sym name)
  "Compare a symbol's name to a string, case-insensitively."
  (and (symbolp sym) (string-equal (symbol-name sym) name)))

(defun query (g expr)
  "Execute a query expression against graph G."
  (let ((form (first expr)))
    (cond
      ((sym-name-equal form "ASK") (execute-ask g expr))
      ((sym-name-equal form "CONSTRUCT") (execute-construct g expr))
      (t (execute-select g expr)))))

;;; ==========================================================================
;;; ASK
;;; ==========================================================================

(defun execute-ask (g expr)
  "ASK returns T if the WHERE pattern has at least one match."
  (let ((body (cdr expr))
        (where-patterns nil))
    (dolist (clause body)
      (when (sym-name-equal (first clause) "WHERE")
        (setf where-patterns (rest clause))))
    (not (null (match-with-paths g (expand-property-paths g where-patterns))))))

;;; ==========================================================================
;;; CONSTRUCT
;;; ==========================================================================

(defun execute-construct (g expr)
  "CONSTRUCT generates triples from query results."
  (let ((template (second expr))
        (body (cddr expr))
        (where-patterns nil)
        (target-graph nil))
    (dolist (clause body)
      (let ((tag (first clause)))
        (cond
          ((sym-name-equal tag "WHERE") (setf where-patterns (rest clause)))
          ((sym-name-equal tag "INTO") (setf target-graph (second clause))))))
    (let ((envs (match-with-paths g (expand-property-paths g where-patterns)))
          (results nil))
      (dolist (env envs)
        (let ((s (subst-vars (first template) env))
              (p (subst-vars (second template) env))
              (o (subst-vars (third template) env)))
          (push (list s p o) results)
          (when target-graph
            (add-triple target-graph s p o))))
      (nreverse results))))

;;; ==========================================================================
;;; SELECT
;;; ==========================================================================

(defun execute-select (g expr)
  "Execute a SELECT query."
  (let* ((select-clause (first expr))
         (vars (second expr))
         (body (cddr expr))
         (where-patterns nil)
         (optional-patterns nil)
         (union-clauses nil)
         (filters nil)
         (binds nil)
         (not-exists-patterns nil)
         (minus-patterns nil)
         (group-var nil)
         (order-var nil)
         (limit-n nil)
         (offset-n nil)
         (distinct-p (sym-name-equal select-clause "SELECT-DISTINCT")))
    ;; Parse clauses
    (dolist (clause body)
      (let ((tag (first clause)))
        (cond
          ((sym-name-equal tag "WHERE") (setf where-patterns (rest clause)))
          ((sym-name-equal tag "OPTIONAL") (setf optional-patterns (rest clause)))
          ((sym-name-equal tag "UNION") (setf union-clauses (rest clause)))
          ((sym-name-equal tag "FILTER") (setf filters (rest clause)))
          ((sym-name-equal tag "BIND") (push (rest clause) binds))
          ((sym-name-equal tag "NOT-EXISTS") (setf not-exists-patterns (rest clause)))
          ((sym-name-equal tag "MINUS") (setf minus-patterns (rest clause)))
          ((sym-name-equal tag "GROUP-BY") (setf group-var (second clause)))
          ((sym-name-equal tag "ORDER-BY") (setf order-var (second clause)))
          ((sym-name-equal tag "LIMIT") (setf limit-n (second clause)))
          ((sym-name-equal tag "OFFSET") (setf offset-n (second clause))))))
    ;; Expand property paths in where-patterns
    (setf where-patterns (expand-property-paths g where-patterns))
    ;; Execute pattern matching
    (let ((envs (if union-clauses
                    (execute-union g union-clauses)
                    (match-with-paths g where-patterns))))
      ;; Apply optional patterns
      (when optional-patterns
        (setf envs (apply-optional g envs optional-patterns)))
      ;; Apply NOT EXISTS
      (when not-exists-patterns
        (setf envs (apply-not-exists g envs not-exists-patterns)))
      ;; Apply MINUS
      (when minus-patterns
        (setf envs (apply-minus g envs minus-patterns)))
      ;; Apply BIND
      (dolist (bind (nreverse binds))
        (setf envs (apply-bind envs (first bind) (second bind))))
      ;; Apply filters
      (when filters
        (setf envs (apply-filters envs filters)))
      ;; GROUP BY + aggregation
      (when group-var
        (return-from execute-select
          (execute-group-by envs group-var vars)))
      ;; Project variables
      (let ((results (project-results vars envs)))
        (when distinct-p
          (setf results (remove-duplicates results :test #'equal)))
        (when order-var
          (let ((var-idx (if (eq vars '*) 0
                             (position order-var vars))))
            (when var-idx
              (setf results (sort results #'result-less-than
                                  :key (lambda (r) (nth var-idx r)))))))
        (when offset-n
          (setf results (nthcdr offset-n results)))
        (when limit-n
          (setf results (subseq results 0 (min limit-n (length results)))))
        results))))

(defun result-less-than (a b)
  (cond
    ((and (numberp a) (numberp b)) (< a b))
    ((and (stringp a) (stringp b)) (string< a b))
    (t (string< (princ-to-string a) (princ-to-string b)))))

;;; ==========================================================================
;;; Projection
;;; ==========================================================================

(defun project-results (vars envs)
  "Extract selected variables from binding environments."
  (cond
    ((eq vars '*)
     (mapcar (lambda (env) (mapcar #'cdr env)) envs))
    ;; Simple (count ?var)
    ((and (= 1 (length vars))
          (listp (first vars))
          (sym-name-equal (first (first vars)) "COUNT"))
     (list (list (length envs))))
    (t
     (mapcar (lambda (env)
               (mapcar (lambda (v) (lookup-binding v env)) vars))
             envs))))

;;; ==========================================================================
;;; FILTER
;;; ==========================================================================

(defun apply-filters (envs filters)
  (remove-if-not
   (lambda (env)
     (every (lambda (f) (eval-filter f env)) filters))
   envs))

(defun eval-filter (filter env)
  (safe-eval (subst-vars filter env)))

(defun safe-eval (expr)
  "Evaluate EXPR using only whitelisted operations."
  (cond
    ((atom expr) expr)
    (t (let ((op (first expr))
             (args (mapcar #'safe-eval (rest expr))))
         (cond
           ((member op '(< > <= >= = /= + - * /
                         equal equalp eql string= string-equal
                         search string< string>
                         numberp stringp symbolp integerp floatp
                         concatenate))
            (apply (symbol-function op) args))
           ((eq op 'not) (not (first args)))
           (t (error "Disallowed filter operation: ~A" op)))))))

(defun subst-vars (expr env)
  (cond
    ((variable-p expr) (or (lookup-binding expr env) expr))
    ((atom expr) expr)
    (t (mapcar (lambda (x) (subst-vars x env)) expr))))

;;; ==========================================================================
;;; UNION
;;; ==========================================================================

(defun execute-union (g union-clauses)
  (let ((all-envs nil))
    (dolist (clause union-clauses all-envs)
      (when (sym-name-equal (first clause) "WHERE")
        (setf all-envs (append all-envs (match-patterns g (rest clause))))))))

;;; ==========================================================================
;;; OPTIONAL
;;; ==========================================================================

(defun apply-optional (g envs patterns)
  (let ((results nil))
    (dolist (env envs results)
      (let ((extended (match-patterns-with-envs g patterns (list env))))
        (if extended
            (dolist (e extended) (push e results))
            (push env results))))))

(defun match-patterns-with-envs (g patterns envs)
  (let ((current envs))
    (dolist (pattern patterns current)
      (let ((new-envs nil))
        (dolist (env current)
          (dolist (m (match-pattern-with-env g pattern env))
            (push m new-envs)))
        (setf current new-envs)))))

;;; ==========================================================================
;;; BIND
;;; ==========================================================================

(defun apply-bind (envs var expr)
  "Add a computed binding to each environment."
  (mapcar (lambda (env)
            (let ((value (eval (subst-vars expr env))))
              (cons (cons var value) env)))
          envs))

;;; ==========================================================================
;;; NOT EXISTS / MINUS
;;; ==========================================================================

(defun apply-not-exists (g envs patterns)
  "Remove envs where the pattern matches."
  (remove-if
   (lambda (env)
     (match-patterns-with-envs g patterns (list env)))
   envs))

(defun apply-minus (g envs patterns)
  "Remove envs where the pattern produces matching bindings for shared variables."
  (remove-if
   (lambda (env)
     (let ((matches (match-patterns-with-envs g patterns (list env))))
       ;; Check if any match binds the shared variables to the same values
       (some (lambda (m)
               (every (lambda (binding)
                        (let ((existing (assoc (car binding) env)))
                          (or (null existing)
                              (equal (cdr existing) (cdr binding)))))
                      m))
             matches)))
   envs))

;;; ==========================================================================
;;; GROUP BY + Aggregation
;;; ==========================================================================

(defun execute-group-by (envs group-var vars)
  "Group environments by GROUP-VAR and compute aggregations."
  (let ((groups (make-hash-table :test 'equal)))
    ;; Partition envs into groups
    (dolist (env envs)
      (let ((key (lookup-binding group-var env)))
        (push env (gethash key groups))))
    ;; Compute aggregations per group
    (let ((results nil))
      (maphash
       (lambda (key group-envs)
         (let ((row (list key)))
           ;; Process each var in the select list after the group var
           (dolist (v (rest vars))
             (if (and (listp v) (>= (length v) 2))
                 ;; Aggregation: (count ?x), (sum ?x), (avg ?x), (min ?x), (max ?x)
                 (let ((agg-fn (first v))
                       (agg-var (second v)))
                   (let ((values (mapcar (lambda (env) (lookup-binding agg-var env))
                                         group-envs)))
                     (push (compute-aggregate agg-fn values) row)))
                 ;; Plain variable — take first value
                 (push (lookup-binding v (first group-envs)) row)))
           (push (nreverse row) results)))
       groups)
      results)))

(defun compute-aggregate (fn values)
  "Compute an aggregate function over a list of values."
  (let ((nums (remove-if-not #'numberp values)))
    (cond
      ((sym-name-equal fn "COUNT") (length values))
      ((sym-name-equal fn "SUM") (reduce #'+ nums :initial-value 0))
      ((sym-name-equal fn "AVG")
       (if nums (/ (reduce #'+ nums) (length nums)) 0))
      ((sym-name-equal fn "MIN") (when nums (reduce #'min nums)))
      ((sym-name-equal fn "MAX") (when nums (reduce #'max nums)))
      (t (error "Unknown aggregate function: ~A" fn)))))

;;; ==========================================================================
;;; Property Paths
;;; ==========================================================================

(defun expand-property-paths (g patterns)
  "Expand property path patterns into executable form.
A property path pattern has a list as predicate: (+ pred), (? pred), (alt p1 p2), (range pred min max)."
  (let ((expanded nil))
    (dolist (pattern patterns (nreverse expanded))
      (destructuring-bind (s p o) pattern
        (if (and (listp p) (symbolp (first p)))
            ;; Property path — expand into a special marker
            (push (list s (list :path-expr p) o) expanded)
            (push pattern expanded))))))

(defun path-pattern-p (pattern)
  "Check if a pattern contains a property path expression."
  (and (listp (second pattern))
       (eq :path-expr (first (second pattern)))))

;;; In execute-select, property paths are handled by expanding them
;;; before calling match-patterns, and applying path patterns after.

(defun match-with-paths (g patterns)
  "Match patterns, handling property paths separately."
  (let ((simple nil)
        (path-pats nil))
    (dolist (p patterns)
      (if (path-pattern-p p)
          (push p path-pats)
          (push p simple)))
    (let ((envs (if simple
                    (match-patterns g (nreverse simple))
                    (list nil))))
      (dolist (pp (nreverse path-pats))
        (setf envs (apply-path-pattern g envs pp)))
      envs)))

(defun apply-path-pattern (g envs pattern)
  "Apply a property path pattern to existing environments."
  (destructuring-bind (s (_ path-expr) o) pattern
    (declare (ignore _))
    (let ((path-op (first path-expr))
          (results nil))
      (dolist (env envs results)
        (let ((rs (resolve s env))
              (ro (resolve o env)))
          (let ((matches (execute-path g rs path-op path-expr ro)))
            (dolist (pair matches)
              (let ((new-env env) (ok t))
                (multiple-value-bind (e success) (unify s (car pair) new-env)
                  (if success (setf new-env e) (setf ok nil)))
                (when ok
                  (multiple-value-bind (e success) (unify o (cdr pair) new-env)
                    (if success (setf new-env e) (setf ok nil))))
                (when ok (push new-env results))))))))))

(defun execute-path (g start op path-expr target)
  "Execute a property path, returning (start . end) pairs."
  (cond
    ;; Transitive closure: (+ "pred")
    ((sym-name-equal op "+")
     (let ((pred (second path-expr)))
       (transitive-closure g start pred target)))
    ;; Optional (zero or one): (? "pred")
    ((sym-name-equal op "?")
     (let ((pred (second path-expr)))
       (zero-or-one-path g start pred target)))
    ;; Alternative: (alt "p1" "p2" ...)
    ((sym-name-equal op "ALT")
     (let ((preds (rest path-expr)))
       (alternative-path g start preds target)))
    ;; Bounded: (range "pred" min max)
    ((sym-name-equal op "RANGE")
     (let ((pred (second path-expr))
           (min-hops (third path-expr))
           (max-hops (fourth path-expr)))
       (bounded-path g start pred min-hops max-hops target)))
    (t (error "Unknown path operator: ~A" op))))

(defun transitive-closure (g start pred target)
  "Find all nodes reachable via one or more hops of PRED."
  (let ((visited (make-hash-table :test 'equal))
        (results nil)
        (bound-start (and (not (variable-p start)) start)))
    (if bound-start
        ;; Forward traversal from known start
        (labels ((walk (node)
                   (dolist (tr (get-triples g :subject node :predicate pred))
                     (let ((next (triple-object tr)))
                       (unless (gethash next visited)
                         (setf (gethash next visited) t)
                         (push (cons bound-start next) results)
                         (walk next))))))
          (walk bound-start))
        ;; Unbound start — try all subjects
        (dolist (subj (all-subjects g))
          (let ((sub-visited (make-hash-table :test 'equal)))
            (labels ((walk (node)
                       (dolist (tr (get-triples g :subject node :predicate pred))
                         (let ((next (triple-object tr)))
                           (unless (gethash next sub-visited)
                             (setf (gethash next sub-visited) t)
                             (push (cons subj next) results)
                             (walk next))))))
              (walk subj)))))
    ;; Filter by target if bound
    (if (and target (not (variable-p target)))
        (remove-if-not (lambda (pair) (equal (cdr pair) target)) results)
        results)))

(defun zero-or-one-path (g start pred target)
  "Zero or one hop: includes the start node itself."
  (let ((bound-start (and (not (variable-p start)) start))
        (results nil))
    (when bound-start
      ;; Zero hops: start itself
      (push (cons bound-start bound-start) results)
      ;; One hop
      (dolist (tr (get-triples g :subject bound-start :predicate pred))
        (push (cons bound-start (triple-object tr)) results)))
    (if (and target (not (variable-p target)))
        (remove-if-not (lambda (pair) (equal (cdr pair) target)) results)
        results)))

(defun alternative-path (g start preds target)
  "Match any of the given predicates."
  (let ((bound-start (and (not (variable-p start)) start))
        (results nil))
    (when bound-start
      (dolist (pred preds)
        (dolist (tr (get-triples g :subject bound-start :predicate pred))
          (push (cons bound-start (triple-object tr)) results))))
    (if (and target (not (variable-p target)))
        (remove-if-not (lambda (pair) (equal (cdr pair) target)) results)
        results)))

(defun bounded-path (g start pred min-hops max-hops target)
  "Match paths with length between MIN-HOPS and MAX-HOPS."
  (let ((bound-start (and (not (variable-p start)) start))
        (results nil))
    (when bound-start
      (let ((current (list bound-start)))
        (dotimes (hop max-hops)
          (let ((next nil))
            (dolist (node current)
              (dolist (tr (get-triples g :subject node :predicate pred))
                (pushnew (triple-object tr) next :test #'equal)))
            (when (>= (1+ hop) min-hops)
              (dolist (n next)
                (pushnew (cons bound-start n) results :test #'equal)))
            (setf current next)))))
    (if (and target (not (variable-p target)))
        (remove-if-not (lambda (pair) (equal (cdr pair) target)) results)
        results)))
