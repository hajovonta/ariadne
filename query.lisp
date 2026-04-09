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
      ((sym-name-equal form "DESCRIBE") (execute-describe g expr))
      ((or (sym-name-equal form "SELECT")
           (sym-name-equal form "SELECT-DISTINCT"))
       (execute-select g expr))
      (t (error "Unknown query form: ~A" form)))))

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
;;; DESCRIBE
;;; ==========================================================================

(defun execute-describe (g expr)
  "DESCRIBE returns all triples about a resource."
  (let ((resource (second expr))
        (mode (third expr)))
    (if (and mode (sym-name-equal mode "SUBJECT"))
        (get-triples g :subject resource)
        ;; Default: triples where resource is subject OR object
        (let ((as-subject (get-triples g :subject resource))
              (as-object (get-triples g :object resource)))
          (remove-duplicates (append as-subject as-object))))))

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
         (values-clause nil)
         (group-var nil)
         (having-clause nil)
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
          ((sym-name-equal tag "VALUES") (setf values-clause (rest clause)))
          ((sym-name-equal tag "GROUP-BY") (setf group-var (second clause)))
          ((sym-name-equal tag "HAVING") (setf having-clause (rest clause)))
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
      ;; Apply VALUES
      (when values-clause
        (setf envs (apply-values-clause envs values-clause)))
      ;; Apply BIND
      (dolist (bind (nreverse binds))
        (setf envs (apply-bind envs (first bind) (second bind))))
      ;; Apply filters (resolve subqueries in filter expressions first)
      (when filters
        (let ((*query-graph* g)
              (resolved-filters (mapcar (lambda (f) (resolve-subquery-in-filter g f)) filters)))
          (setf envs (apply-filters envs resolved-filters))))

      ;; GROUP BY + aggregation
      (when group-var
        (return-from execute-select
          (execute-group-by envs group-var vars having-clause)))
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
    ;; Aggregation without GROUP BY: (select ((count ?var)) ...) or (select ((max ?var)) ...)
    ((and (= 1 (length vars))
          (listp (first vars))
          (>= (length (first vars)) 2)
          (let ((fn-name (symbol-name (first (first vars)))))
            (member fn-name '("COUNT" "SUM" "AVG" "MIN" "MAX") :test #'string-equal)))
     (let* ((agg-spec (first vars))
            (agg-var (second agg-spec))
            (values (mapcar (lambda (env) (lookup-binding agg-var env)) envs)))
       (list (list (compute-aggregate (first agg-spec) values)))))
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

(defvar *query-graph* nil "Dynamic binding for graph during filter evaluation.")

(defun safe-eval (expr)
  "Evaluate EXPR using only whitelisted operations."
  (cond
    ((atom expr) expr)
    ((null (first expr)) (error "Disallowed filter operation: NIL"))
    ((sym-name-equal (first expr) "SUBQUERY")
     (let ((results (query *query-graph* (second expr))))
       (if (and results (= 1 (length results)) (= 1 (length (first results))))
           (caar results)
           results)))
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
           ((sym-name-equal op "REGEX")
            (apply #'ariadne-regex args))
           (t (error "Disallowed filter operation: ~A" op)))))))

(defun ariadne-regex (string pattern &optional mode)
  "Regex match using cl-ppcre."
  (let ((scanner (if (and mode (sym-name-equal mode "CASE-INSENSITIVE-MODE"))
                     (cl-ppcre:create-scanner pattern :case-insensitive-mode t)
                     (cl-ppcre:create-scanner pattern))))
    (not (null (cl-ppcre:scan scanner string)))))

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

(defun execute-group-by (envs group-var vars &optional having-clause)
  "Group environments by GROUP-VAR and compute aggregations."
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (env envs)
      (let ((key (lookup-binding group-var env)))
        (push env (gethash key groups))))
    (let ((results nil))
      (maphash
       (lambda (key group-envs)
         ;; Check HAVING before including this group
         (when (or (null having-clause)
                   (eval-having having-clause group-envs vars))
           (let ((row (list key)))
             (dolist (v (rest vars))
               (if (and (listp v) (>= (length v) 2))
                   (let ((agg-var (second v)))
                     (let ((values (mapcar (lambda (env) (lookup-binding agg-var env))
                                           group-envs)))
                       (push (compute-aggregate (first v) values) row)))
                   (push (lookup-binding v (first group-envs)) row)))
             (push (nreverse row) results))))
       groups)
      results)))

(defun eval-having (having-clause group-envs vars)
  "Evaluate a HAVING clause against a group of environments."
  (every
   (lambda (expr)
     (let ((resolved (subst-having-aggregates expr group-envs vars)))
       (eval resolved)))
   having-clause))

(defun subst-having-aggregates (expr group-envs vars)
  "Replace aggregate expressions in HAVING with computed values."
  (cond
    ((atom expr) expr)
    ;; Recognize (count ?var), (sum ?var), etc.
    ((and (symbolp (first expr))
          (member (symbol-name (first expr))
                  '("COUNT" "SUM" "AVG" "MIN" "MAX")
                  :test #'string-equal)
          (= 2 (length expr))
          (variable-p (second expr)))
     (let ((values (mapcar (lambda (env) (lookup-binding (second expr) env))
                           group-envs)))
       (compute-aggregate (first expr) values)))
    (t (mapcar (lambda (x) (subst-having-aggregates x group-envs vars)) expr))))

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
  (let ((expanded nil))
    (dolist (pattern patterns (nreverse expanded))
      (if (or (subquery-pattern-p pattern)
              (/= 3 (length pattern)))
          (push pattern expanded)
          (destructuring-bind (s p o) pattern
            (if (and (listp p) (symbolp (first p)))
                (push (list s (list :path-expr p) o) expanded)
                (push pattern expanded)))))))

(defun path-pattern-p (pattern)
  "Check if a pattern contains a property path expression."
  (and (listp (second pattern))
       (eq :path-expr (first (second pattern)))))

;;; In execute-select, property paths are handled by expanding them
;;; before calling match-patterns, and applying path patterns after.

(defun match-with-paths (g patterns)
  "Match patterns, handling property paths and subqueries."
  (let ((simple nil)
        (path-pats nil)
        (subquery-pats nil))
    (dolist (p patterns)
      (cond
        ((path-pattern-p p) (push p path-pats))
        ((subquery-pattern-p p) (push p subquery-pats))
        (t (push p simple))))
    (let ((envs (if simple
                    (match-patterns g (nreverse simple))
                    (list nil))))
      (dolist (pp (nreverse path-pats))
        (setf envs (apply-path-pattern g envs pp)))
      (dolist (sq (nreverse subquery-pats))
        (setf envs (apply-subquery-pattern g envs sq)))
      envs)))

(defun subquery-pattern-p (pattern)
  (and (listp pattern)
       (symbolp (first pattern))
       (sym-name-equal (first pattern) "SUBQUERY")))

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
    ((sym-name-equal op "+")
     (transitive-closure g start (second path-expr) target))
    ((sym-name-equal op "?")
     (zero-or-one-path g start (second path-expr) target))
    ((sym-name-equal op "*")
     (kleene-star-path g start (second path-expr) target))
    ((sym-name-equal op "ALT")
     (alternative-path g start (rest path-expr) target))
    ((sym-name-equal op "RANGE")
     (bounded-path g start (second path-expr) (third path-expr) (fourth path-expr) target))
    ((sym-name-equal op "INV")
     (inverse-path g start (second path-expr) target))
    ((sym-name-equal op "INV+")
     (inverse-transitive g start (second path-expr) target))
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

(defun kleene-star-path (g start pred target)
  "Zero or more hops (Kleene star)."
  (let ((bound-start (and (not (variable-p start)) start))
        (results nil))
    (when bound-start
      ;; Zero hops: start itself
      (push (cons bound-start bound-start) results)
      ;; One or more: transitive closure
      (let ((visited (make-hash-table :test 'equal)))
        (setf (gethash bound-start visited) t)
        (labels ((walk (node)
                   (dolist (tr (get-triples g :subject node :predicate pred))
                     (let ((next (triple-object tr)))
                       (unless (gethash next visited)
                         (setf (gethash next visited) t)
                         (push (cons bound-start next) results)
                         (walk next))))))
          (walk bound-start))))
    (if (and target (not (variable-p target)))
        (remove-if-not (lambda (pair) (equal (cdr pair) target)) results)
        results)))

(defun inverse-path (g start pred target)
  "Inverse path: follow edges backwards."
  (let ((bound-start (and (not (variable-p start)) start))
        (results nil))
    (when bound-start
      (dolist (tr (get-triples g :predicate pred :object bound-start))
        (push (cons bound-start (triple-subject tr)) results)))
    (if (and target (not (variable-p target)))
        (remove-if-not (lambda (pair) (equal (cdr pair) target)) results)
        results)))

(defun inverse-transitive (g start pred target)
  "Inverse transitive: follow edges backwards, one or more hops."
  (let ((bound-start (and (not (variable-p start)) start))
        (results nil))
    (when bound-start
      (let ((visited (make-hash-table :test 'equal)))
        (labels ((walk (node)
                   (dolist (tr (get-triples g :predicate pred :object node))
                     (let ((next (triple-subject tr)))
                       (unless (gethash next visited)
                         (setf (gethash next visited) t)
                         (push (cons bound-start next) results)
                         (walk next))))))
          (walk bound-start))))
    (if (and target (not (variable-p target)))
        (remove-if-not (lambda (pair) (equal (cdr pair) target)) results)
        results)))

;;; ==========================================================================
;;; VALUES
;;; ==========================================================================

(defun apply-values-clause (envs clause)
  "Filter envs to only those matching VALUES bindings.
CLAUSE is either (?var (val1 val2 ...)) or ((?v1 ?v2) ((a b) (c d) ...))."
  (let ((var-spec (first clause))
        (data (second clause)))
    (if (variable-p var-spec)
        ;; Single variable: (values ?x ("a" "b" "c"))
        (remove-if-not
         (lambda (env)
           (let ((val (lookup-binding var-spec env)))
             (member val data :test #'equal)))
         envs)
        ;; Multiple variables: (values (?x ?y) (("a" "b") ("c" "d")))
        (remove-if-not
         (lambda (env)
           (some (lambda (row)
                   (every (lambda (var val)
                            (equal (lookup-binding var env) val))
                          var-spec row))
                 data))
         envs))))

;;; ==========================================================================
;;; Subqueries
;;; ==========================================================================

(defun apply-subquery-pattern (g envs pattern)
  "Apply a subquery pattern: (subquery <query-expr> <bind-var>)."
  (let ((sq-expr (second pattern))
        (bind-var (third pattern)))
    (let ((sq-results (query g sq-expr)))
      (let ((sq-values (mapcar (lambda (row)
                                 (if (= 1 (length row)) (first row) row))
                               sq-results)))
        (remove-if-not
         (lambda (env)
           (let ((val (lookup-binding bind-var env)))
             (member val sq-values :test #'equal)))
         envs)))))

;;; Subqueries in FILTER — handled by safe-eval recognizing (subquery ...) forms

(defun resolve-subquery-in-filter (g expr)
  "Pre-process filter expressions to resolve embedded subqueries."
  (cond
    ((atom expr) expr)
    ((sym-name-equal (first expr) "SUBQUERY")
     ;; (subquery (select ...)) => the scalar result
     (let ((results (query g (second expr))))
       (if (and results (= 1 (length results)) (= 1 (length (first results))))
           (caar results)
           results)))
    (t (mapcar (lambda (x) (resolve-subquery-in-filter g x)) expr))))
