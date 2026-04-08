;;;; pattern.lisp
;;;; SPARQL-like pattern matching with logic variables

(in-package #:ariadne)

;;; ==========================================================================
;;; Variables and Bindings
;;; ==========================================================================

(defun variable-p (x)
  "A logic variable is a symbol starting with ?"
  (and (symbolp x)
       (> (length (symbol-name x)) 0)
       (char= #\? (char (symbol-name x) 0))))

(defun lookup-binding (var env)
  (cdr (assoc var env)))

;;; ==========================================================================
;;; Unification
;;; ==========================================================================

(defun unify (pattern value env)
  "Unify PATTERN with VALUE given bindings ENV.
Returns two values: the updated ENV and a success boolean."
  (cond
    ((variable-p pattern)
     (let ((binding (assoc pattern env)))
       (if binding
           (if (equal (cdr binding) value)
               (values env t)
               (values nil nil))
           (values (cons (cons pattern value) env) t))))
    ((equal pattern value) (values env t))
    (t (values nil nil))))

(defmacro unify-or-fail (pattern value env-var ok-var)
  "Try to unify; on failure set OK-VAR to nil."
  `(when ,ok-var
     (multiple-value-bind (.e .s) (unify ,pattern ,value ,env-var)
       (if .s (setf ,env-var .e) (setf ,ok-var nil)))))

;;; ==========================================================================
;;; Pattern Matching
;;; ==========================================================================

(defun match-pattern (g pattern)
  "Match a single triple pattern against the graph.
Returns a list of binding environments."
  (destructuring-bind (ps pp po) pattern
    (let ((bound-s (and (not (variable-p ps)) ps))
          (bound-p (and (not (variable-p pp)) pp))
          (bound-o (and (not (variable-p po)) po)))
      (let ((candidates (get-triples g :subject bound-s :predicate bound-p :object bound-o))
            (results nil))
        (dolist (tr candidates results)
          (let ((env nil) (ok t))
            (unify-or-fail ps (triple-subject tr) env ok)
            (unify-or-fail pp (triple-predicate tr) env ok)
            (unify-or-fail po (triple-object tr) env ok)
            (when ok (push env results))))))))

(defun match-patterns (g patterns)
  "Match multiple triple patterns, joining on shared variables."
  (let ((envs (list nil)))  ; one empty environment
    (dolist (pattern patterns envs)
      (let ((new-envs nil))
        (dolist (env envs)
          (dolist (new-env (match-pattern-with-env g pattern env))
            (push new-env new-envs)))
        (setf envs new-envs)))))

(defun match-pattern-with-env (g pattern env)
  "Match a pattern against the graph, using existing bindings from ENV."
  (destructuring-bind (ps pp po) pattern
    (let ((rs (resolve ps env))
          (rp (resolve pp env))
          (ro (resolve po env)))
      (let ((bound-s (and (not (variable-p rs)) rs))
            (bound-p (and (not (variable-p rp)) rp))
            (bound-o (and (not (variable-p ro)) ro)))
        (let ((candidates (get-triples g :subject bound-s :predicate bound-p :object bound-o))
              (results nil))
          (dolist (tr candidates results)
            (let ((new-env env) (ok t))
              (unify-or-fail rs (triple-subject tr) new-env ok)
              (unify-or-fail rp (triple-predicate tr) new-env ok)
              (unify-or-fail ro (triple-object tr) new-env ok)
              (when ok (push new-env results)))))))))

(defun resolve (x env)
  "If X is a bound variable in ENV, return its value; otherwise return X."
  (if (variable-p x)
      (let ((binding (assoc x env)))
        (if binding (cdr binding) x))
      x))
