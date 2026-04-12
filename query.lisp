;;;; query.lisp
;;;; SPARQL-like query DSL

(in-package #:ariadne)

;;; ==========================================================================
;;; Query Engine
;;; ==========================================================================

(defun sym-name-equal (sym name)
  "Compare a symbol's name to a string, case-insensitively."
  (and (symbolp sym) (string-equal (symbol-name sym) name)))

(defmethod query ((g graph) expr)
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
  "ASK returns T if the WHERE pattern has at least one match (after filters)."
  (let ((body (cdr expr))
        (where-patterns nil)
        (filters nil)
        (binds nil))
    (dolist (clause body)
      (let ((tag (first clause)))
        (cond
          ((sym-name-equal tag "WHERE") (setf where-patterns (rest clause)))
          ((sym-name-equal tag "FILTER") (setf filters (rest clause)))
          ((sym-name-equal tag "BIND") (push (rest clause) binds)))))
    (let ((envs (if where-patterns
                    (match-with-paths g (expand-property-paths g where-patterns))
                    (list nil))))
      (dolist (bind (nreverse binds))
        (setf envs (apply-bind envs (first bind) (second bind))))
      (when filters
        (let ((*query-graph* g))
          (setf envs (apply-filters envs filters))))
      (not (null envs)))))

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
         (exists-patterns nil)
         (minus-patterns nil)
         (minus-filters nil)
         (values-clause nil)
         (graph-clause nil)
         (projections nil)
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
          ((sym-name-equal tag "EXISTS") (setf exists-patterns (rest clause)))
          ((sym-name-equal tag "MINUS")
           (let ((parts (rest clause)))
             (setf minus-patterns (remove-if (lambda (p) (and (consp p) (sym-name-equal (car p) "FILTER"))) parts))
             (let ((f (find-if (lambda (p) (and (consp p) (sym-name-equal (car p) "FILTER"))) parts)))
               (when f (setf minus-filters (rest f))))))
          ((sym-name-equal tag "VALUES") (setf values-clause (rest clause)))
          ((sym-name-equal tag "GRAPH")
           (setf graph-clause (rest clause)))
          ((sym-name-equal tag "PROJECT") (push (rest clause) projections))
          ((sym-name-equal tag "GROUP-BY") (setf group-var (second clause)))
          ((sym-name-equal tag "HAVING") (setf having-clause (rest clause)))
          ((sym-name-equal tag "ORDER-BY") (setf order-var (second clause)))
          ((sym-name-equal tag "LIMIT") (setf limit-n (second clause)))
          ((sym-name-equal tag "OFFSET") (setf offset-n (second clause))))))
    ;; Expand property paths in where-patterns
    (setf where-patterns (expand-property-paths g where-patterns))
    ;; Execute pattern matching
    (let ((envs (cond
                 (union-clauses (execute-union g union-clauses))
                 (graph-clause
                  (let* ((graph-name (first graph-clause))
                         (graph-patterns (second graph-clause))
                         (patterns-to-use (or graph-patterns where-patterns)))
                    (match-in-graph g patterns-to-use graph-name)))
                 (where-patterns (match-with-paths g where-patterns))
                 (t (list nil)))))
      ;; Apply optional patterns
      (when optional-patterns
        (setf envs (apply-optional g envs optional-patterns)))
      ;; Apply NOT EXISTS
      (when not-exists-patterns
        (setf envs (apply-not-exists g envs not-exists-patterns)))
      ;; Apply EXISTS (keep only envs where pattern matches)
      (when exists-patterns
        (setf envs (remove-if-not
                    (lambda (env)
                      (match-patterns-with-envs g exists-patterns (list env)))
                    envs)))
      ;; Apply MINUS
      (when minus-patterns
        (setf envs (apply-minus g envs minus-patterns minus-filters)))
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

      ;; Apply projections (SELECT (expr AS ?var))
      (dolist (proj projections)
        (let ((alias (first proj))
              (expr (second proj)))
          (if (and (consp expr) (member (car expr) '(COUNT SUM AVG MIN MAX GROUP_CONCAT SAMPLE)
                                        :test #'sym-name-equal))
              ;; Aggregate without GROUP BY: compute over all results
              (unless group-var
                (let* ((agg-var (cadr expr))
                       (values (mapcar (lambda (env) (lookup-binding agg-var env)) envs))
                       (agg-result (compute-aggregate (car expr) values)))
                  (setf envs (list (list (cons alias agg-result))))))
              ;; Non-aggregate: compute per row like BIND
              (setf envs (apply-bind envs alias expr)))))
      ;; GROUP BY + aggregation
      (when group-var
        (return-from execute-select
          (execute-group-by envs group-var vars having-clause projections)))
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
    ((eq (first expr) 'quote) (second expr))
    ;; IN needs special handling — second arg is a list of exprs, not a function call
    ((sym-name-equal (first expr) "IN")
     (let ((val (safe-eval (second expr)))
           (list-vals (mapcar (lambda (e) (handler-case (safe-eval e) (error () :error)))
                              (third expr))))
       (member val (remove :error list-vals) :test #'equal)))
    ((sym-name-equal (first expr) "SUBQUERY")
     (let ((results (query *query-graph* (second expr))))
       (if (and results (= 1 (length results)) (= 1 (length (first results))))
           (caar results)
           results)))
    (t (let ((op (first expr))
             (args (mapcar #'safe-eval (rest expr))))
         (cond
           ((member op '(+ - * /
                         equal equalp eql string= string-equal
                         search string< string>
                         numberp stringp symbolp integerp floatp
                         concatenate))
            (apply (symbol-function op) args))
           ((eq op '=) (equal (first args) (second args)))
           ((eq op '/=) (not (equal (first args) (second args))))
           ((eq op '<) (shacl-safe-compare #'< #'string< (first args) (second args)))
           ((eq op '>) (shacl-safe-compare #'> #'string> (first args) (second args)))
           ((eq op '<=) (shacl-safe-compare #'<= #'string<= (first args) (second args)))
           ((eq op '>=) (shacl-safe-compare #'>= #'string>= (first args) (second args)))
           ((eq op 'not) (not (first args)))
           ((eq op 'and) (and (first args) (second args)))
           ((eq op 'or) (or (first args) (second args)))
           ((sym-name-equal op "BOUND") (not (null (first args))))
           ((sym-name-equal op "ISNUMERIC") (numberp (first args)))
           ((sym-name-equal op "ISLITERAL")
            (let ((v (first args)))
              (or (stringp v) (numberp v) (member v '(t nil)))))
           ((sym-name-equal op "ISIRI")
            (let ((v (first args)))
              (and (stringp v) (search "://" v))))
           ((sym-name-equal op "ISBLANK")
            (let ((v (first args)))
              (and (stringp v) (>= (length v) 2)
                   (char= #\_ (char v 0)) (char= #\: (char v 1)))))
           ((sym-name-equal op "LANG")
            (let ((v (first args)))
              (if (and (stringp v) (position #\@ v))
                  (subseq v (1+ (position #\@ v)))
                  "")))
           ((sym-name-equal op "LANGMATCHES")
            (let ((tag (first args))
                  (range (second args)))
              (and (stringp tag) (stringp range)
                   (or (string= range "*")
                       (string-equal tag range)
                       (and (> (length tag) (length range))
                            (char= #\- (char tag (length range)))
                            (string-equal (subseq tag 0 (length range)) range))))))
           ((sym-name-equal op "CONCAT")
            (apply #'concatenate 'string (mapcar #'princ-to-string args)))
           ((sym-name-equal op "CONCATENATE")
            (apply #'concatenate (first args) (rest args)))
           ((sym-name-equal op "STR") (princ-to-string (first args)))
           ((sym-name-equal op "STRLEN") (length (princ-to-string (first args))))
           ((sym-name-equal op "UCASE") (string-upcase (princ-to-string (first args))))
           ((sym-name-equal op "LCASE") (string-downcase (princ-to-string (first args))))
           ((sym-name-equal op "SUBSTR")
            (let ((s (princ-to-string (first args)))
                  (start (1- (truncate (second args))))  ; SPARQL is 1-based
                  (len (third args)))
              (if len (subseq s start (min (+ start (truncate len)) (length s)))
                  (subseq s start))))
           ((sym-name-equal op "CONTAINS")
            (search (princ-to-string (second args)) (princ-to-string (first args)) :test #'char=))
           ((sym-name-equal op "STRSTARTS")
            (let ((s (princ-to-string (first args))) (p (princ-to-string (second args))))
              (and (>= (length s) (length p)) (string= s p :end1 (length p)))))
           ((sym-name-equal op "STRENDS")
            (let ((s (princ-to-string (first args))) (p (princ-to-string (second args))))
              (and (>= (length s) (length p))
                   (string= s p :start1 (- (length s) (length p))))))
           ((sym-name-equal op "REPLACE")
            (cl-ppcre:regex-replace-all (second args) (princ-to-string (first args)) (third args)))
           ((sym-name-equal op "STRBEFORE")
            (let* ((s (princ-to-string (first args))) (p (princ-to-string (second args)))
                   (pos (search p s)))
              (if pos (subseq s 0 pos) "")))
           ((sym-name-equal op "STRAFTER")
            (let* ((s (princ-to-string (first args))) (p (princ-to-string (second args)))
                   (pos (search p s)))
              (if pos (subseq s (+ pos (length p))) "")))
           ((sym-name-equal op "ENCODE_FOR_URI")
            (with-output-to-string (out)
              (loop for c across (first args) do
                (if (or (alphanumericp c) (member c '(#\_ #\. #\~ #\-)))
                    (write-char c out)
                    (format out "%~2,'0X" (char-code c))))))
           ((sym-name-equal op "IF")
            (if (first args) (second args) (third args)))
           ((sym-name-equal op "COALESCE")
            (find-if #'identity args))
           ((sym-name-equal op "DATATYPE")
            (let ((v (first args)))
              (cond ((integerp v) "http://www.w3.org/2001/XMLSchema#integer")
                    ((floatp v) "http://www.w3.org/2001/XMLSchema#double")
                    ((stringp v) "http://www.w3.org/2001/XMLSchema#string")
                    ((member v '(t nil)) "http://www.w3.org/2001/XMLSchema#boolean")
                    (t ""))))
           ((or (sym-name-equal op "URI") (sym-name-equal op "IRI"))
            (princ-to-string (first args)))
           ((sym-name-equal op "STRLANG")
            (format nil "~A@~A" (first args) (second args)))
           ((sym-name-equal op "STRDT")
            (first args))  ; simplified — just return the value
           ((sym-name-equal op "ABS") (abs (first args)))
           ((sym-name-equal op "ROUND") (round (first args)))
           ((sym-name-equal op "CEIL") (ceiling (first args)))
           ((sym-name-equal op "FLOOR") (floor (first args)))
           ((sym-name-equal op "YEAR")
            (if (stringp (first args))
                (parse-integer (subseq (first args) 0 (position #\- (first args))) :junk-allowed t)
                0))
           ((sym-name-equal op "MONTH")
            (if (stringp (first args))
                (let ((s (first args))) (parse-integer (subseq s 5 7) :junk-allowed t))
                0))
           ((sym-name-equal op "DAY")
            (if (stringp (first args))
                (let ((s (first args))) (parse-integer (subseq s 8 10) :junk-allowed t))
                0))
           ((sym-name-equal op "HOURS")
            (if (and (stringp (first args)) (position #\T (first args)))
                (let* ((s (first args)) (t-pos (position #\T s)))
                  (parse-integer (subseq s (1+ t-pos) (+ t-pos 3)) :junk-allowed t))
                0))
           ((sym-name-equal op "MINUTES")
            (if (and (stringp (first args)) (position #\T (first args)))
                (let* ((s (first args)) (t-pos (position #\T s)))
                  (parse-integer (subseq s (+ t-pos 4) (+ t-pos 6)) :junk-allowed t))
                0))
           ((sym-name-equal op "SECONDS")
            (if (and (stringp (first args)) (position #\T (first args)))
                (let* ((s (first args)) (t-pos (position #\T s)))
                  (parse-integer (subseq s (+ t-pos 7)) :junk-allowed t))
                0))
           ((sym-name-equal op "MD5")
            (ironclad:byte-array-to-hex-string
             (ironclad:digest-sequence :md5 (sb-ext:string-to-octets (princ-to-string (first args)) :external-format :utf-8))))
           ((sym-name-equal op "SHA1")
            (ironclad:byte-array-to-hex-string
             (ironclad:digest-sequence :sha1 (sb-ext:string-to-octets (princ-to-string (first args)) :external-format :utf-8))))
           ((sym-name-equal op "SHA256")
            (ironclad:byte-array-to-hex-string
             (ironclad:digest-sequence :sha256 (sb-ext:string-to-octets (princ-to-string (first args)) :external-format :utf-8))))
           ((sym-name-equal op "SHA384")
            (ironclad:byte-array-to-hex-string
             (ironclad:digest-sequence :sha384 (sb-ext:string-to-octets (princ-to-string (first args)) :external-format :utf-8))))
           ((sym-name-equal op "SHA512")
            (ironclad:byte-array-to-hex-string
             (ironclad:digest-sequence :sha512 (sb-ext:string-to-octets (princ-to-string (first args)) :external-format :utf-8))))
           ((sym-name-equal op "NOW")
            (multiple-value-bind (sec min hr day mon yr) (get-decoded-time)
              (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" yr mon day hr min sec)))
           ((sym-name-equal op "RAND") (random 1.0d0))
           ((sym-name-equal op "UUID")
            (format nil "urn:uuid:~8,'0X-~4,'0X-~4,'0X-~4,'0X-~12,'0X"
                    (random (expt 2 32)) (random (expt 2 16)) (random (expt 2 16))
                    (random (expt 2 16)) (random (expt 2 48))))
           ((sym-name-equal op "STRUUID")
            (format nil "~8,'0X-~4,'0X-~4,'0X-~4,'0X-~12,'0X"
                    (random (expt 2 32)) (random (expt 2 16)) (random (expt 2 16))
                    (random (expt 2 16)) (random (expt 2 48))))
           ((sym-name-equal op "BNODE")
            (format nil "_:b~A" (random (expt 2 32))))
           ;; XSD cast functions
           ((cl-ppcre:scan "^XSD:" (symbol-name op))
            (let ((type (subseq (symbol-name op) 4))
                  (v (first args)))
              (cond
                ((string-equal type "STRING") (princ-to-string v))
                ((string-equal type "INTEGER") (truncate (if (stringp v) (read-from-string v) v)))
                ((string-equal type "DECIMAL") (float (if (stringp v) (read-from-string v) v)))
                ((string-equal type "FLOAT") (float (if (stringp v) (read-from-string v) v)))
                ((string-equal type "DOUBLE") (float (if (stringp v) (read-from-string v) v) 1.0d0))
                ((string-equal type "BOOLEAN")
                 (cond ((member v '(t nil)) v)
                       ((stringp v) (not (or (string= v "") (string= v "0") (string-equal v "false"))))
                       ((numberp v) (not (zerop v)))
                       (t nil)))
                (t v))))
           ((sym-name-equal op "REGEX")
            (apply #'ariadne-regex args))
           (t (error "Disallowed filter operation: ~A" op)))))))

(defun shacl-safe-compare (num-fn str-fn a b)
  (cond
    ((and (numberp a) (numberp b)) (funcall num-fn a b))
    ((and (stringp a) (stringp b)) (funcall str-fn a b))
    (t nil)))

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
            (let ((value (handler-case (safe-eval (subst-vars expr env))
                           (error () nil))))
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

(defun apply-minus (g envs patterns &optional filters)
  "Remove envs where the pattern produces matching bindings for shared variables."
  (remove-if
   (lambda (env)
     (let ((matches (match-patterns-with-envs g patterns (list env))))
       (when filters
         (setf matches (remove-if-not
                        (lambda (m)
                          (every (lambda (f) (safe-eval (subst-vars f m))) filters))
                        matches)))
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

(defun execute-group-by (envs group-var vars &optional having-clause projections)
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
               (let ((proj (find v projections :key #'first :test #'equal)))
                 (if proj
                     ;; Projected aggregate
                     (let* ((expr (second proj))
                            (agg-var (when (consp expr) (cadr expr)))
                            (values (when agg-var
                                      (mapcar (lambda (env) (lookup-binding agg-var env))
                                              group-envs))))
                       (push (if values (compute-aggregate (car expr) values) nil) row))
                     ;; Regular variable or old-style (AGG ?var)
                     (if (and (listp v) (>= (length v) 2))
                         (let ((values (mapcar (lambda (env) (lookup-binding (second v) env))
                                               group-envs))
                               (separator (third v)))
                           (push (compute-aggregate (first v) values separator) row))
                         (push (lookup-binding v (first group-envs)) row)))))
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

(defun compute-aggregate (fn values &optional separator)
  "Compute an aggregate function over a list of values."
  (let ((nums (remove-if-not #'numberp values)))
    (cond
      ((sym-name-equal fn "COUNT") (length values))
      ((sym-name-equal fn "SUM") (reduce #'+ nums :initial-value 0))
      ((sym-name-equal fn "AVG")
       (if nums (/ (reduce #'+ nums) (length nums)) 0))
      ((sym-name-equal fn "MIN") (when nums (reduce #'min nums)))
      ((sym-name-equal fn "MAX") (when nums (reduce #'max nums)))
      ((sym-name-equal fn "GROUP-CONCAT")
       (let ((sep (or separator ", ")))
         (format nil (concatenate 'string "~{~A~^" sep "~}")
                 (mapcar #'princ-to-string values))))
      ((sym-name-equal fn "SAMPLE")
       (first values))
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
  "Match patterns, handling property paths, subqueries, and SERVICE."
  (let ((simple nil)
        (path-pats nil)
        (subquery-pats nil)
        (service-pats nil))
    (dolist (p patterns)
      (cond
        ((service-pattern-p p) (push p service-pats))
        ((path-pattern-p p) (push p path-pats))
        ((subquery-pattern-p p) (push p subquery-pats))
        (t (push p simple))))
    ;; Reorder simple patterns by selectivity (most bound positions first)
    (when simple
      (setf simple (optimize-pattern-order (nreverse simple))))
    (let ((envs (if simple
                    (match-patterns g simple)
                    (list nil))))
      (dolist (pp (nreverse path-pats))
        (setf envs (apply-path-pattern g envs pp)))
      (dolist (sq (nreverse subquery-pats))
        (setf envs (apply-subquery-pattern g envs sq)))
      (dolist (sp (nreverse service-pats))
        (setf envs (apply-service-pattern envs sp)))
      envs)))

(defun optimize-pattern-order (patterns)
  "Reorder patterns so more selective ones execute first.
Selectivity = number of bound (non-variable) positions."
  (let ((bound-vars (make-hash-table :test 'equal)))
    ;; Greedy: pick the most selective pattern at each step,
    ;; considering variables bound by previously selected patterns
    (let ((remaining patterns)
          (ordered nil))
      (loop while remaining do
        (let ((best nil)
              (best-score -1))
          (dolist (p remaining)
            (let ((score (pattern-selectivity p bound-vars)))
              (when (> score best-score)
                (setf best p best-score score))))
          (push best ordered)
          (setf remaining (remove best remaining :test #'eq))
          ;; Mark variables in this pattern as bound for subsequent patterns
          (dolist (term (if (= 3 (length best)) best nil))
            (when (variable-p term)
              (setf (gethash term bound-vars) t)))))
      (nreverse ordered))))

(defun pattern-selectivity (pattern bound-vars)
  "Score a pattern's selectivity. Higher = more selective = should run first.
Bound constants score 2, variables already bound by prior patterns score 1, unbound variables score 0."
  (let ((score 0))
    (when (= 3 (length pattern))
      (dolist (term pattern)
        (cond
          ((not (variable-p term)) (incf score 2))  ; constant
          ((gethash term bound-vars) (incf score 1)) ; bound by prior pattern
          (t nil))))                                   ; unbound
    score))

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
    ((sym-name-equal op "ZEROORONE")
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
    ((sym-name-equal op "SEQ")
     (sequence-path g start (rest path-expr) target))
    ((sym-name-equal op "NEG")
     (negated-property-set g start (second path-expr) target))
    (t (error "Unknown path operator: ~A" op))))

(defun negated-property-set (g start excluded-pred target)
  "Match any predicate EXCEPT the excluded one(s)."
  (let ((bound-start (and (not (variable-p start)) start))
        (excluded (if (listp excluded-pred) excluded-pred (list excluded-pred)))
        (results nil))
    (when bound-start
      (dolist (tr (get-triples g :subject bound-start))
        (unless (member (triple-predicate tr) excluded :test #'equal)
          (push (cons bound-start (triple-object tr)) results))))
    (if (and target (not (variable-p target)))
        (remove-if-not (lambda (pair) (equal (cdr pair) target)) results)
        results)))

(defun one-hop (g node pred)
  "Get all nodes reachable from NODE via one application of PRED (simple or complex path)."
  (if (and (listp pred) (symbolp (first pred)))
      (mapcar #'cdr (execute-path g node (first pred) pred nil))
      (mapcar #'triple-object (get-triples g :subject node :predicate pred))))

(defun transitive-closure (g start pred target)
  "Find all nodes reachable via one or more hops of PRED."
  (let ((visited (make-hash-table :test 'equal))
        (results nil)
        (bound-start (and (not (variable-p start)) start)))
    (if bound-start
        (labels ((walk (node)
                   (dolist (next (one-hop g node pred))
                     (unless (gethash next visited)
                       (setf (gethash next visited) t)
                       (push (cons bound-start next) results)
                       (walk next)))))
          (walk bound-start))
        (dolist (subj (all-subjects g))
          (let ((sub-visited (make-hash-table :test 'equal)))
            (labels ((walk (node)
                       (dolist (next (one-hop g node pred))
                         (unless (gethash next sub-visited)
                           (setf (gethash next sub-visited) t)
                           (push (cons subj next) results)
                           (walk next)))))
              (walk subj)))))
    (if (and target (not (variable-p target)))
        (remove-if-not (lambda (pair) (equal (cdr pair) target)) results)
        results)))

(defun zero-or-one-path (g start pred target)
  "Zero or one hop: includes the start node itself."
  (let ((bound-start (and (not (variable-p start)) start))
        (results nil))
    (when bound-start
      (push (cons bound-start bound-start) results)
      (dolist (next (one-hop g bound-start pred))
        (push (cons bound-start next) results)))
    (if (and target (not (variable-p target)))
        (remove-if-not (lambda (pair) (equal (cdr pair) target)) results)
        results)))

(defun alternative-path (g start preds target)
  "Match any of the given predicates."
  (let ((bound-start (and (not (variable-p start)) start))
        (results nil))
    (when bound-start
      (dolist (pred preds)
        (if (and (listp pred) (symbolp (first pred)))
            ;; Nested path expression
            (dolist (pair (execute-path g bound-start (first pred) pred nil))
              (push pair results))
            ;; Simple predicate
            (dolist (tr (get-triples g :subject bound-start :predicate pred))
              (push (cons bound-start (triple-object tr)) results)))))
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
      (push (cons bound-start bound-start) results)
      (let ((visited (make-hash-table :test 'equal)))
        (setf (gethash bound-start visited) t)
        (labels ((walk (node)
                   (dolist (next (one-hop g node pred))
                     (unless (gethash next visited)
                       (setf (gethash next visited) t)
                       (push (cons bound-start next) results)
                       (walk next)))))
          (walk bound-start))))
    (if (and target (not (variable-p target)))
        (remove-if-not (lambda (pair) (equal (cdr pair) target)) results)
        results)))

(defun inverse-path (g start pred target)
  "Inverse path: follow edges backwards."
  (let ((bound-start (and (not (variable-p start)) start))
        (results nil))
    (when bound-start
      (if (and (listp pred) (symbolp (first pred)))
          ;; Complex path — find all nodes that reach start via forward path
          (dolist (subj (all-subjects g))
            (let ((fwd (execute-path g subj (first pred) pred nil)))
              (when (find bound-start fwd :key #'cdr :test #'equal)
                (push (cons bound-start subj) results))))
          ;; Simple predicate
          (dolist (tr (get-triples g :predicate pred :object bound-start))
            (push (cons bound-start (triple-subject tr)) results))))
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

(defun sequence-path (g start predicates target)
  "Sequence path: chain multiple predicates. (seq p1 p2 p3) = p1/p2/p3."
  (let ((bound-start (and (not (variable-p start)) start))
        (results nil))
    (when bound-start
      (let ((current (list bound-start)))
        (dolist (step predicates)
          (let ((next nil))
            (dolist (node current)
              (if (and (listp step) (symbolp (first step)))
                  (dolist (pair (execute-path g node (first step) step nil))
                    (push (cdr pair) next))
                  (dolist (tr (get-triples g :subject node :predicate step))
                    (push (triple-object tr) next))))
            (setf current next)))
        (dolist (end current)
          (push (cons bound-start end) results))))
    (if (and target (not (variable-p target)))
        (remove-if-not (lambda (pair) (equal (cdr pair) target)) results)
        results)))

(defun apply-graph-patterns (g outer-envs graph-patterns graph-name)
  "Match graph-patterns against named graph, joining with outer environments."
  (let ((graph-envs (match-in-graph g graph-patterns graph-name)))
    ;; Join outer and graph environments
    (if (and outer-envs graph-envs)
        (let ((results nil))
          (dolist (oe outer-envs)
            (dolist (ge graph-envs)
              (push (append ge oe) results)))
          (nreverse results))
        graph-envs)))

(defun match-in-graph (g patterns graph-name)
  "Match patterns only against triples in the named graph.
Falls back to default graph if named graph is empty."
  (let ((quads (get-quads g :graph graph-name)))
    (if quads
        (let ((temp (make-graph)))
          (dolist (tr quads)
            (add-triple temp (triple-subject tr) (triple-predicate tr) (triple-object tr)))
          (match-with-paths temp patterns))
        ;; Fallback: query default graph
        (match-with-paths g patterns))))


;;; ==========================================================================
;;; Query Cursor / Pagination
;;; ==========================================================================

(defstruct (query-cursor (:constructor %make-query-cursor))
  results
  (offset 0 :type fixnum)
  (page-size 10 :type fixnum))

(defun make-query-cursor (g expr &key (page-size 10))
  "Create a cursor for paginated query results."
  ;; Strip any existing limit/offset from expr and execute full query
  (let ((clean-expr (remove-if (lambda (clause)
                                 (and (listp clause)
                                      (member (car clause) '(limit offset))))
                               expr)))
    (%make-query-cursor :results (query g clean-expr)
                        :page-size page-size)))

(defun cursor-next (cursor)
  "Return the next page of results from CURSOR."
  (let* ((results (query-cursor-results cursor))
         (offset (query-cursor-offset cursor))
         (size (query-cursor-page-size cursor))
         (page (subseq results
                       (min offset (length results))
                       (min (+ offset size) (length results)))))
    (setf (query-cursor-offset cursor) (+ offset size))
    page))

(defun cursor-done-p (cursor)
  "Return T if all results have been consumed."
  (>= (query-cursor-offset cursor)
      (length (query-cursor-results cursor))))

;;; ==========================================================================
;;; SPARQL SERVICE (federated queries)
;;; ==========================================================================

(defun service-pattern-p (p)
  "Return T if P is a SERVICE pattern."
  (and (listp p) (eq (first p) 'service)))

(defun patterns-to-sparql (patterns vars)
  "Serialize triple patterns and variable list into a SPARQL SELECT string."
  (format nil "SELECT ~{~A ~}WHERE { ~{~A~} }"
          (mapcar (lambda (v) (format nil "?~A" (string-downcase (subseq (symbol-name v) 1))))
                  vars)
          (mapcar (lambda (pat)
                    (format nil "~A ~A ~A . "
                            (term-to-sparql (first pat))
                            (term-to-sparql (second pat))
                            (term-to-sparql (third pat))))
                  patterns)))

(defun term-to-sparql (term)
  "Convert a query term to SPARQL syntax."
  (cond
    ((and (symbolp term) (char= #\? (char (symbol-name term) 0)))
     (format nil "?~A" (string-downcase (subseq (symbol-name term) 1))))
    ((stringp term) (format nil "<~A>" term))
    (t (format nil "~A" term))))

(defun query-remote-sparql (url sparql-string)
  "Send a SPARQL query to a remote endpoint, return list of binding alists."
  (multiple-value-bind (body status)
      (drakma:http-request url
                           :method :get
                           :parameters (list (cons "query" sparql-string))
                           :accept "application/json")
    (when (/= status 200)
      (return-from query-remote-sparql nil))
    (let* ((json (jzon:parse (if (stringp body) body
                                 (flexi-streams:octets-to-string body :external-format :utf-8))))
           (results (gethash "results" json)))
      (when results
        (map 'list
             (lambda (row)
               (let ((bindings nil))
                 (loop for i from 0 below (length row) do
                   (push (aref row i) bindings))
                 (nreverse bindings)))
             results)))))

(defun collect-variables (patterns)
  "Collect all ?variables from a list of triple patterns."
  (let ((vars nil))
    (dolist (pat patterns)
      (dolist (term pat)
        (when (and (symbolp term) (char= #\? (char (symbol-name term) 0)))
          (pushnew term vars))))
    (nreverse vars)))

(defun apply-service-pattern (envs service-pat)
  "Execute a SERVICE pattern against a remote endpoint and join with local bindings."
  (let* ((url (second service-pat))
         (patterns (third service-pat))
         (svc-vars (collect-variables patterns))
         (result-envs nil))
    (dolist (env envs)
      ;; Substitute bound variables into patterns
      (let ((bound-patterns
              (mapcar (lambda (pat)
                        (mapcar (lambda (term)
                                  (if (and (symbolp term)
                                           (char= #\? (char (symbol-name term) 0))
                                           (assoc term env))
                                      (cdr (assoc term env))
                                      term))
                                pat))
                      patterns)))
        ;; Build query with remaining unbound vars
        (let* ((unbound (remove-if (lambda (v) (assoc v env)) svc-vars))
               (all-vars (or unbound svc-vars))
               (query-str (patterns-to-sparql bound-patterns all-vars))
               (remote-results (query-remote-sparql url query-str)))
          (if remote-results
              (dolist (row remote-results)
                (let ((new-env (copy-list env)))
                  ;; Bind unbound vars from remote results
                  (loop for var in all-vars
                        for val in row
                        do (unless (assoc var new-env)
                             (push (cons var val) new-env)))
                  (push new-env result-envs)))
              ;; No results from remote — this env is dropped (inner join)
              ))))
    (nreverse result-envs)))
