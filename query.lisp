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
  (let* ((select-clause (first expr))
         (vars (second expr))
         (body (cddr expr))
         (where-patterns nil)
         (optional-patterns nil)
         (union-clauses nil)
         (filters nil)
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
          ((sym-name-equal tag "ORDER-BY") (setf order-var (second clause)))
          ((sym-name-equal tag "LIMIT") (setf limit-n (second clause)))
          ((sym-name-equal tag "OFFSET") (setf offset-n (second clause))))))
    ;; Execute pattern matching
    (let ((envs (if union-clauses
                    (execute-union g union-clauses)
                    (match-patterns g where-patterns))))
      ;; Apply optional patterns
      (when optional-patterns
        (setf envs (apply-optional g envs optional-patterns)))
      ;; Apply filters
      (when filters
        (setf envs (apply-filters envs filters)))
      ;; Project variables
      (let ((results (project-results vars envs)))
        ;; Distinct
        (when distinct-p
          (setf results (remove-duplicates results :test #'equal)))
        ;; Order by
        (when order-var
          (let ((var-idx (if (eq vars '*)
                             0
                             (position order-var vars))))
            (when var-idx
              (setf results (sort results #'result-less-than
                                  :key (lambda (r) (nth var-idx r)))))))
        ;; Offset
        (when offset-n
          (setf results (nthcdr offset-n results)))
        ;; Limit
        (when limit-n
          (setf results (subseq results 0 (min limit-n (length results)))))
        results))))

(defun result-less-than (a b)
  (cond
    ((and (numberp a) (numberp b)) (< a b))
    ((and (stringp a) (stringp b)) (string< a b))
    (t (string< (princ-to-string a) (princ-to-string b)))))

(defun project-results (vars envs)
  "Extract selected variables from binding environments."
  (cond
    ;; (select * ...)
    ((eq vars '*)
     (mapcar (lambda (env)
               (mapcar #'cdr env))
             envs))
    ;; (select ((count ?var)) ...)
    ((and (= 1 (length vars))
          (listp (first vars))
          (sym-name-equal (first (first vars)) "COUNT"))
     (list (list (length envs))))
    ;; (select (?x ?y ...) ...)
    (t
     (mapcar (lambda (env)
               (mapcar (lambda (v) (lookup-binding v env)) vars))
             envs))))

(defun apply-filters (envs filters)
  "Keep only environments where all filter conditions are true."
  (remove-if-not
   (lambda (env)
     (every (lambda (filter) (eval-filter filter env)) filters))
   envs))

(defun eval-filter (filter env)
  "Evaluate a filter expression with variables resolved from ENV."
  (let ((resolved (subst-vars filter env)))
    (eval resolved)))

(defun subst-vars (expr env)
  "Substitute all ?variables in EXPR with their values from ENV."
  (cond
    ((variable-p expr) (or (lookup-binding expr env) expr))
    ((atom expr) expr)
    (t (mapcar (lambda (x) (subst-vars x env)) expr))))

(defun execute-union (g union-clauses)
  "Execute UNION: combine results from multiple WHERE clauses."
  (let ((all-envs nil))
    (dolist (clause union-clauses all-envs)
      (when (sym-name-equal (first clause) "WHERE")
        (let ((envs (match-patterns g (rest clause))))
          (setf all-envs (append all-envs envs)))))))

(defun apply-optional (g envs patterns)
  "For each env, try to extend it with optional patterns.
If the optional doesn't match, keep the original env."
  (let ((results nil))
    (dolist (env envs results)
      (let ((extended (match-patterns-with-envs g patterns (list env))))
        (if extended
            (dolist (e extended) (push e results))
            (push env results))))))

(defun match-patterns-with-envs (g patterns envs)
  "Match patterns starting from existing environments."
  (let ((current envs))
    (dolist (pattern patterns current)
      (let ((new-envs nil))
        (dolist (env current)
          (let ((matches (match-pattern-with-env g pattern env)))
            (dolist (m matches) (push m new-envs))))
        (setf current new-envs)))))
