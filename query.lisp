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
      ;; Template: list of triples from SPARQL, or single triple from s-expr API
      (let ((tmpl (if (and (= 3 (length template))
                       (not (consp (first template))))
                      (list template)  ; single triple → wrap
                      template)))      ; already list of triples
        (dolist (env envs)
          (dolist (tp tmpl)
            (let ((s (subst-vars (first tp) env))
                  (p (subst-vars (second tp) env))
                  (o (subst-vars (third tp) env)))
              (when (and s p o (not (variable-p s)) (not (variable-p p)) (not (variable-p o)))
                (push (list s p o) results)
                (when target-graph
                  (add-triple target-graph s p o)))))))
      (remove-duplicates (nreverse results) :test #'equal))))

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
         (optional-groups nil)
         (union-clauses nil)
         (filters nil)
         (binds nil)
         (not-exists-patterns nil)
         (exists-patterns nil)
         (minus-clauses nil)
         (values-clause nil)
         (graph-clause nil)
         (projections nil)
         (group-var nil)
         (group-exprs nil)
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
          ((sym-name-equal tag "OPTIONAL") (push (rest clause) optional-groups))
          ((sym-name-equal tag "UNION") (setf union-clauses (rest clause)))
          ((sym-name-equal tag "FILTER") (setf filters (rest clause)))
          ((sym-name-equal tag "BIND") (push (rest clause) binds))
          ((sym-name-equal tag "NOT-EXISTS") (setf not-exists-patterns (rest clause)))
          ((sym-name-equal tag "EXISTS") (setf exists-patterns (rest clause)))
          ((sym-name-equal tag "MINUS")
           (let* ((parts (rest clause))
                  (pats (remove-if (lambda (p) (and (consp p) (sym-name-equal (car p) "FILTER"))) parts))
                  (f (find-if (lambda (p) (and (consp p) (sym-name-equal (car p) "FILTER"))) parts))
                  (filts (when f (rest f))))
             (push (list pats filts) minus-clauses)))
          ((sym-name-equal tag "VALUES") (setf values-clause (rest clause)))
          ((sym-name-equal tag "GRAPH")
           (setf graph-clause (rest clause)))
          ((sym-name-equal tag "PROJECT") (push (rest clause) projections))
          ((sym-name-equal tag "GROUP-BY") (setf group-var (second clause)))
          ((sym-name-equal tag "GROUP-BY-MULTI") (setf group-var (second clause)))
          ((sym-name-equal tag "GROUP-BY-EXPR")
           (setf group-var (second clause))
           (push (list (second clause) (third clause)) group-exprs))
          ((sym-name-equal tag "HAVING")
           (let ((h (second clause)))
             ;; Normalize: single expr → list of one, list of exprs → as-is
             (setf having-clause (if (and (consp h) (consp (first h)))
                                     h (list h)))))
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
      (dolist (opt-pats (nreverse optional-groups))
        (setf envs (apply-optional g envs opt-pats)))
      ;; Apply NOT EXISTS
      (when not-exists-patterns
        (setf envs (apply-not-exists g envs not-exists-patterns)))
      ;; Apply EXISTS (keep only envs where pattern matches)
      (when exists-patterns
        (setf envs (remove-if-not
                    (lambda (env)
                      (match-patterns-with-envs g exists-patterns (list env)))
                    envs)))
      ;; Apply MINUS (each clause independently)
      (dolist (mc (nreverse minus-clauses))
        (setf envs (apply-minus g envs (first mc) (second mc))))
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
          (if (and (consp expr) (member (car expr)
                                        '(COUNT SUM AVG MIN MAX GROUP_CONCAT SAMPLE
                                          COUNT-DISTINCT SUM-DISTINCT AVG-DISTINCT MIN-DISTINCT
                                          MAX-DISTINCT GROUP_CONCAT-DISTINCT SAMPLE-DISTINCT)
                                        :test #'sym-name-equal))
              ;; Aggregate without GROUP BY: compute over all results
              (unless group-var
                (let* ((agg-var (cadr expr))
                       (sep (caddr expr))
                       (values (if (sym-name-equal agg-var "*")
                                   envs
                                   (mapcar (lambda (env) (lookup-binding agg-var env)) envs)))
                       (agg-result (compute-aggregate (car expr) values sep)))
                  (setf envs (list (list (cons alias agg-result))))))
              ;; Non-aggregate: compute per row like BIND
              (setf envs (apply-bind envs alias expr)))))
      ;; GROUP BY + aggregation
      (when group-var
        ;; Apply GROUP BY expressions (compute and bind alias)
        (dolist (ge group-exprs)
          (setf envs (mapcar (lambda (env)
                               (let ((val (handler-case
                                              (safe-eval (subst-vars (second ge) env))
                                            (error () nil))))
                                 (acons (first ge) val env)))
                             envs)))
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
    ((or (eq vars '*) (equal vars '("*")))
     ;; Collect all variable bindings
     (let ((all-vars (remove-duplicates
                      (loop for env in envs nconc (mapcar #'car env))
                      :test #'equal)))
       (mapcar (lambda (env)
                 (mapcar (lambda (v) (lookup-binding v env)) all-vars))
               envs)))
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
       (member (lit-val val) (mapcar #'lit-val (remove :error list-vals)) :test #'equal)))
    ((sym-name-equal (first expr) "SUBQUERY")
     (let ((results (query *query-graph* (second expr))))
       (if (and results (= 1 (length results)) (= 1 (length (first results))))
           (caar results)
           results)))
    (t (let ((op (first expr))
             (args (mapcar #'safe-eval (rest expr))))
         (cond
           ((member op '(+ - * /))
            (let ((result (apply (symbol-function op) (mapcar #'lit-val args))))
              (if (numberp result)
                  (intern-literal result (if (integerp result) +xsd-integer+ +xsd-decimal+))
                  result)))
           ((member op '(equal equalp eql string= string-equal
                         search string< string>
                         numberp stringp symbolp integerp floatp
                         concatenate))
            (apply (symbol-function op) (mapcar #'lit-val args)))
           ((eq op '=) (equal (lit-val (first args)) (lit-val (second args))))
           ((eq op '/=) (not (equal (lit-val (first args)) (lit-val (second args)))))
           ((sym-name-equal op "!=") (not (equal (lit-val (first args)) (lit-val (second args)))))
           ((eq op '<) (shacl-safe-compare #'< #'string< (first args) (second args)))
           ((eq op '>) (shacl-safe-compare #'> #'string> (first args) (second args)))
           ((eq op '<=) (shacl-safe-compare #'<= #'string<= (first args) (second args)))
           ((eq op '>=) (shacl-safe-compare #'>= #'string>= (first args) (second args)))
           ((eq op 'not) (not (first args)))
           ((eq op 'and) (and (first args) (second args)))
           ((eq op 'or) (or (first args) (second args)))
           ((sym-name-equal op "BOUND") (not (null (first args))))
           ((sym-name-equal op "ISNUMERIC") (numberp (lit-val (first args))))
           ((sym-name-equal op "ISLITERAL")
            (or (rdf-literal-p (first args))
                (numberp (first args))
                (member (first args) '(t nil))))
           ((sym-name-equal op "ISIRI")
            (let ((v (first args)))
              (and (stringp v) (not (rdf-literal-p v))
                   (not (and (> (length v) 1) (string= "_:" v :end2 2))))))
           ((sym-name-equal op "ISBLANK")
            (let ((v (first args)))
              (and (stringp v) (not (rdf-literal-p v))
                   (>= (length v) 2)
                   (char= #\_ (char v 0)) (char= #\: (char v 1)))))
           ((sym-name-equal op "LANG")
            (let ((v (first args)))
              (if (rdf-literal-p v)
                  (or (rdf-literal-language v) "")
                  "")))
           ((sym-name-equal op "LANGMATCHES")
            (let ((tag (lit-val (first args)))
                  (range (lit-val (second args))))
              (and (stringp tag) (stringp range)
                   (or (string= range "*")
                       (string-equal tag range)
                       (and (> (length tag) (length range))
                            (char= #\- (char tag (length range)))
                            (string-equal (subseq tag 0 (length range)) range))))))
           ((sym-name-equal op "CONCAT")
            (intern-literal (apply #'concatenate 'string
                                   (mapcar (lambda (a) (princ-to-string (lit-val a))) args))
                            +xsd-string+))
           ((sym-name-equal op "CONCATENATE")
            (apply #'concatenate (first args) (rest args)))
           ((sym-name-equal op "STR")
            (let ((v (first args)))
              (intern-literal (if (rdf-literal-p v)
                                  (princ-to-string (rdf-literal-value v))
                                  (princ-to-string v))
                              +xsd-string+)))
           ((sym-name-equal op "STRLEN")
            (intern-literal (length (princ-to-string (first args))) +xsd-integer+))
           ((sym-name-equal op "UCASE")
            (intern-literal (string-upcase (princ-to-string (first args))) +xsd-string+))
           ((sym-name-equal op "LCASE")
            (intern-literal (string-downcase (princ-to-string (first args))) +xsd-string+))
           ((sym-name-equal op "SUBSTR")
            (let ((s (princ-to-string (first args)))
                  (start (1- (truncate (lit-val (second args)))))
                  (len (third args)))
              (intern-literal (if len (subseq s start (min (+ start (truncate (lit-val len))) (length s)))
                                  (subseq s start))
                              +xsd-string+)))
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
            (intern-literal (cl-ppcre:regex-replace-all (lit-val (second args)) (princ-to-string (first args)) (lit-val (third args)))
                            +xsd-string+))
           ((sym-name-equal op "STRBEFORE")
            (let* ((s (princ-to-string (first args))) (p (princ-to-string (second args)))
                   (pos (search p s)))
              (intern-literal (if pos (subseq s 0 pos) "") +xsd-string+)))
           ((sym-name-equal op "STRAFTER")
            (let* ((s (princ-to-string (first args))) (p (princ-to-string (second args)))
                   (pos (search p s)))
              (intern-literal (if pos (subseq s (+ pos (length p))) "") +xsd-string+)))
           ((sym-name-equal op "ENCODE_FOR_URI")
            (intern-literal
             (with-output-to-string (out)
               (loop for c across (lit-val (first args)) do
                 (if (or (alphanumericp c) (member c '(#\_ #\. #\~ #\-)))
                     (write-char c out)
                     (format out "%~2,'0X" (char-code c)))))
             +xsd-string+))
           ((sym-name-equal op "IF")
            (if (first args) (second args) (third args)))
           ((sym-name-equal op "COALESCE")
            (find-if #'identity args))
           ((sym-name-equal op "DATATYPE")
            (let ((v (first args)))
              (cond ((rdf-literal-p v) (rdf-literal-datatype v))
                    ((integerp v) +xsd-integer+)
                    ((floatp v) +xsd-double+)
                    ((stringp v) +xsd-string+)
                    ((member v '(t nil)) +xsd-boolean+)
                    (t ""))))
           ((or (sym-name-equal op "URI") (sym-name-equal op "IRI"))
            (princ-to-string (first args)))
           ((sym-name-equal op "STRLANG")
            (intern-literal (lit-val (first args)) +rdf-langstring+ (lit-val (second args))))
           ((sym-name-equal op "STRDT")
            (intern-literal (lit-val (first args)) (lit-val (second args))))
           ((sym-name-equal op "ABS")
            (let ((r (abs (lit-val (first args)))))
              (intern-literal r (if (integerp r) +xsd-integer+ +xsd-decimal+))))
           ((sym-name-equal op "ROUND")
            (intern-literal (round (lit-val (first args))) +xsd-integer+))
           ((sym-name-equal op "CEIL")
            (intern-literal (ceiling (lit-val (first args))) +xsd-integer+))
           ((sym-name-equal op "FLOOR")
            (intern-literal (floor (lit-val (first args))) +xsd-integer+))
           ((sym-name-equal op "YEAR")
            (let ((s (lit-val (first args))))
              (if (stringp s)
                (parse-integer (subseq s 0 (position #\- s)) :junk-allowed t)
                0)))
           ((sym-name-equal op "MONTH")
            (let ((s (lit-val (first args))))
              (if (stringp s) (parse-integer (subseq s 5 7) :junk-allowed t) 0)))
           ((sym-name-equal op "DAY")
            (let ((s (lit-val (first args))))
              (if (stringp s) (parse-integer (subseq s 8 10) :junk-allowed t) 0)))
           ((sym-name-equal op "HOURS")
            (let ((s (lit-val (first args))))
              (if (and (stringp s) (position #\T s))
                (let ((t-pos (position #\T s)))
                  (parse-integer (subseq s (1+ t-pos) (+ t-pos 3)) :junk-allowed t))
                0)))
           ((sym-name-equal op "MINUTES")
            (let ((s (lit-val (first args))))
              (if (and (stringp s) (position #\T s))
                (let ((t-pos (position #\T s)))
                  (parse-integer (subseq s (+ t-pos 4) (+ t-pos 6)) :junk-allowed t))
                0)))
           ((sym-name-equal op "SECONDS")
            (let ((s (lit-val (first args))))
              (if (and (stringp s) (position #\T s))
                (let ((t-pos (position #\T s)))
                  (parse-integer (subseq s (+ t-pos 7)) :junk-allowed t))
                0)))
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
            (intern-literal
             (multiple-value-bind (sec min hr day mon yr) (get-decoded-time)
               (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" yr mon day hr min sec))
             +xsd-datetime+))
           ((sym-name-equal op "RAND") (random 1.0d0))
           ((sym-name-equal op "UUID")
            (format nil "urn:uuid:~8,'0X-~4,'0X-~4,'0X-~4,'0X-~12,'0X"
                    (random (expt 2 32)) (random (expt 2 16)) (random (expt 2 16))
                    (random (expt 2 16)) (random (expt 2 48))))
           ((sym-name-equal op "STRUUID")
            (intern-literal (format nil "~8,'0X-~4,'0X-~4,'0X-~4,'0X-~12,'0X"
                    (random (expt 2 32)) (random (expt 2 16)) (random (expt 2 16))
                    (random (expt 2 16)) (random (expt 2 48)))
                            +xsd-string+))
           ((sym-name-equal op "BNODE")
            (format nil "_:b~A" (random (expt 2 32))))
           ;; XSD cast functions
           ((cl-ppcre:scan "^XSD:" (symbol-name op))
            (let ((type (subseq (symbol-name op) 4))
                  (v (lit-val (first args))))
              (cond
                ((string-equal type "STRING") (intern-literal (princ-to-string v) +xsd-string+))
                ((string-equal type "INTEGER")
                 (intern-literal (truncate (if (stringp v) (read-from-string v) v)) +xsd-integer+))
                ((string-equal type "DECIMAL")
                 (intern-literal (float (if (stringp v) (read-from-string v) v)) +xsd-decimal+))
                ((string-equal type "FLOAT")
                 (intern-literal (float (if (stringp v) (read-from-string v) v)) +xsd-float+))
                ((string-equal type "DOUBLE")
                 (intern-literal (float (if (stringp v) (read-from-string v) v) 1.0d0) +xsd-double+))
                ((string-equal type "BOOLEAN")
                 (intern-literal (cond ((member v '(t nil)) v)
                                       ((stringp v) (not (or (string= v "") (string= v "0") (string-equal v "false"))))
                                       ((numberp v) (not (zerop v)))
                                       (t nil))
                                 +xsd-boolean+))
                (t (intern-literal v (concatenate 'string "http://www.w3.org/2001/XMLSchema#" type))))))
           ((sym-name-equal op "REGEX")
            (apply #'ariadne-regex (mapcar #'lit-val args)))
           ((sym-name-equal op "IF")
            (if (first args) (second args) (third args)))
           ((or (sym-name-equal op "IRI") (sym-name-equal op "URI"))
            (let ((v (lit-val (first args))))
              (if (stringp v) v (princ-to-string v))))
           (t (error "Disallowed filter operation: ~A" op)))))))

(defun shacl-safe-compare (num-fn str-fn a b)
  (let ((a (lit-val a)) (b (lit-val b)))
    (cond
      ((and (numberp a) (numberp b)) (funcall num-fn a b))
      ((and (stringp a) (stringp b)) (funcall str-fn a b))
      (t nil))))

(defun ariadne-regex (string pattern &optional mode)
  "Regex match using cl-ppcre."
  (let* ((ci (and mode (or (sym-name-equal mode "CASE-INSENSITIVE-MODE")
                           (and (stringp mode) (search "i" mode)))))
         (scanner (cl-ppcre:create-scanner pattern :case-insensitive-mode ci)))
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
      (cond
        ((and (consp pattern) (symbolp (car pattern))
              (sym-name-equal (car pattern) "EXISTS"))
         (setf current (remove-if-not
                        (lambda (env)
                          (match-patterns-with-envs g (rest pattern) (list env)))
                        current)))
        ((and (consp pattern) (symbolp (car pattern))
              (sym-name-equal (car pattern) "NOT-EXISTS"))
         (setf current (apply-not-exists g current (rest pattern))))
        ((and (consp pattern) (symbolp (car pattern))
              (sym-name-equal (car pattern) "FILTER"))
         (setf current (remove-if-not
                        (lambda (env)
                          (handler-case
                              (let ((v (safe-eval (subst-vars (second pattern) env))))
                                (and v (not (equal v ""))))
                            (error () nil)))
                        current)))
        ((and (consp pattern) (symbolp (car pattern))
              (sym-name-equal (car pattern) "VALUES"))
         (setf current (apply-values-clause current (rest pattern))))
        (t (let ((new-envs nil))
             (dolist (env current)
               (dolist (m (match-pattern-with-env g pattern env))
                 (push m new-envs)))
             (setf current new-envs)))))))

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
  ;; Separate optional patterns from basic patterns
  (let ((basic nil) (opts nil))
    (dolist (p patterns)
      (if (and (consp p) (symbolp (car p)) (sym-name-equal (car p) "OPTIONAL"))
          (push (rest p) opts)
          (push p basic)))
    (setf basic (nreverse basic))
    (setf opts (nreverse opts))
    ;; Evaluate MINUS patterns independently (not with outer bindings)
    (let ((minus-envs (match-with-paths g basic)))
      (dolist (opt opts)
        (setf minus-envs (apply-optional g minus-envs opt)))
      (when filters
        (setf minus-envs (remove-if-not
                          (lambda (m)
                            (every (lambda (f) (safe-eval (subst-vars f m))) filters))
                          minus-envs)))
      ;; Remove outer envs that have compatible shared bindings with any minus env
      (remove-if
       (lambda (env)
         (some (lambda (m)
                 (let ((shared nil) (compat t))
                   (dolist (b m)
                     (let ((e (assoc (car b) env)))
                       (when (and e (cdr b))
                         (push t shared)
                         (unless (equal (cdr e) (cdr b))
                           (setf compat nil)))))
                   (and shared compat)))
               minus-envs))
       envs))))

;;; ==========================================================================
;;; GROUP BY + Aggregation
;;; ==========================================================================

(defun execute-group-by (envs group-var vars &optional having-clause projections)
  "Group environments by GROUP-VAR (single var or list of vars) and compute aggregations."
  (let ((groups (make-hash-table :test 'equal))
        (multi-p (listp group-var)))
    (dolist (env envs)
      (let ((key (if multi-p
                     (mapcar (lambda (v) (lit-val (lookup-binding v env))) group-var)
                     (lit-val (lookup-binding group-var env)))))
        (push env (gethash key groups))))
    (let ((results nil))
      (maphash
       (lambda (key group-envs)
         ;; Check HAVING before including this group
         (when (or (null having-clause)
                   (eval-having having-clause group-envs vars))
           (let ((row (if multi-p (copy-list key) (list key))))
             (let ((remaining-vars (if multi-p
                                       (nthcdr (length group-var) vars)
                                       (rest vars))))
             (dolist (v remaining-vars)
               (let ((proj (find v projections :key #'first :test #'equal)))
                 (if proj
                     ;; Projected aggregate
                     (let* ((expr (second proj))
                            (agg-var (when (consp expr) (cadr expr)))
                            (sep (when (consp expr) (caddr expr)))
                            (values (when agg-var
                                      (if (sym-name-equal agg-var "*")
                                          group-envs
                                          (mapcar (lambda (env) (lookup-binding agg-var env))
                                                  group-envs)))))
                       (push (if values (compute-aggregate (car expr) values sep) nil) row))
                     ;; Regular variable or old-style (AGG ?var)
                     (if (and (listp v) (>= (length v) 2))
                         (let ((values (mapcar (lambda (env) (lookup-binding (second v) env))
                                               group-envs))
                               (separator (third v)))
                           (push (compute-aggregate (first v) values separator) row))
                         (push (lookup-binding v (first group-envs)) row))))))
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
                  '("COUNT" "SUM" "AVG" "MIN" "MAX" "GROUP_CONCAT" "SAMPLE"
                    "COUNT-DISTINCT" "SUM-DISTINCT" "AVG-DISTINCT" "MIN-DISTINCT" "MAX-DISTINCT"
                    "GROUP_CONCAT-DISTINCT" "SAMPLE-DISTINCT")
                  :test #'string-equal)
          (>= (length expr) 2))
     (let* ((agg-var (second expr))
            (values (if (or (sym-name-equal agg-var "*") (not (variable-p agg-var)))
                        group-envs
                        (mapcar (lambda (env) (lookup-binding agg-var env))
                                group-envs))))
       (compute-aggregate (first expr) values (third expr))))
    (t (mapcar (lambda (x) (subst-having-aggregates x group-envs vars)) expr))))

(defun compute-aggregate (fn values &optional separator)
  "Compute an aggregate function over a list of values."
  (let* ((distinct-p (and (symbolp fn) (search "DISTINCT" (symbol-name fn))))
         (base-fn (if distinct-p
                      (intern (subseq (symbol-name fn) 0 (search "-DISTINCT" (symbol-name fn))))
                      fn))
         (vals (if distinct-p (remove-duplicates values :test #'equal) values))
         (nums (remove nil (mapcar (lambda (v) (let ((n (lit-val v))) (when (numberp n) n))) vals))))
    (cond
      ((sym-name-equal base-fn "COUNT") (length vals))
      ((sym-name-equal base-fn "SUM") (reduce #'+ nums :initial-value 0))
      ((sym-name-equal base-fn "AVG")
       (if nums (/ (reduce #'+ nums) (length nums)) 0))
      ((sym-name-equal base-fn "MIN") (when nums (reduce #'min nums)))
      ((sym-name-equal base-fn "MAX") (when nums (reduce #'max nums)))
      ((or (sym-name-equal base-fn "GROUP_CONCAT")
           (sym-name-equal base-fn "GROUP-CONCAT"))
       (let* ((sep (or (when (stringp separator) separator)
                       (when (rdf-literal-p separator) (rdf-literal-value separator))
                       " "))
              (strs (mapcar (lambda (v) (princ-to-string (lit-val v))) vals)))
         (format nil "~{~A~}" (loop for (s . rest) on strs
                                     collect s when rest collect sep))))
      ((sym-name-equal base-fn "SAMPLE")
       (first vals))
      (t (error "Unknown aggregate function: ~A" fn)))))

;;; ==========================================================================
;;; Property Paths
;;; ==========================================================================

(defun expand-property-paths (g patterns)
  (let ((expanded nil))
    (dolist (pattern patterns (nreverse expanded))
      (if (or (subquery-pattern-p pattern)
              (/= 3 (length pattern))
              (and (consp pattern) (symbolp (car pattern))
                   (member (symbol-name (car pattern))
                           '("VALUES" "EXISTS" "NOT-EXISTS" "GRAPH" "FILTER" "OPTIONAL" "MINUS" "INLINE-BIND")
                           :test #'string-equal)))
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
  "Match patterns, handling property paths, subqueries, SERVICE, and inline-bind."
  (let ((simple nil)
        (path-pats nil)
        (subquery-pats nil)
        (service-pats nil)
        (inline-binds nil))
    (dolist (p patterns)
      (cond
        ((and (consp p) (symbolp (car p)) (sym-name-equal (car p) "INLINE-BIND"))
         (push p inline-binds))
        ((and (consp p) (symbolp (car p))
              (or (sym-name-equal (car p) "EXISTS")
                  (sym-name-equal (car p) "NOT-EXISTS")
                  (sym-name-equal (car p) "GRAPH")
                  (sym-name-equal (car p) "VALUES")))
         nil) ; handled after basic matching
        ((service-pattern-p p) (push p service-pats))
        ((path-pattern-p p) (push p path-pats))
        ((subquery-pattern-p p) (push p subquery-pats))
        (t (push p simple))))
    ;; If we have inline-binds, process patterns in order with bind interleaving
    (if inline-binds
        (let ((envs (list nil))
              (pre-bind nil)
              (post-bind nil)
              (found-bind nil))
          ;; Split patterns: before first inline-bind and after
          (dolist (p patterns)
            (if (and (consp p) (symbolp (car p)) (sym-name-equal (car p) "INLINE-BIND"))
                (setf found-bind p)
                (if found-bind
                    (push p post-bind)
                    (push p pre-bind))))
          ;; Match pre-bind patterns
          (when pre-bind
            (setf envs (match-with-paths g (nreverse pre-bind))))
          ;; Apply the bind
          (when found-bind
            (setf envs (apply-bind envs (second found-bind) (third found-bind))))
          ;; Match post-bind patterns with bound envs
          (when post-bind
            (let ((post-pats (nreverse post-bind)))
              (setf envs (let ((results nil))
                           (dolist (env envs (apply #'nconc (nreverse results)))
                             (push (match-patterns-with-envs g post-pats (list env)) results))))))
          envs)
        ;; No inline-binds — original logic
        (progn
          (when simple
            (setf simple (optimize-pattern-order (nreverse simple))))
          ;; Collect inline VALUES to seed initial environments
          (let ((init-envs (list nil)))
            (dolist (p patterns)
              (when (and (consp p) (symbolp (car p)) (sym-name-equal (car p) "VALUES"))
                (setf init-envs (apply-values-inline init-envs (rest p)))))
            (let ((envs (if simple
                            (let ((results nil))
                              (dolist (env init-envs (apply #'nconc (nreverse results)))
                                (push (match-patterns-with-envs g simple (list env)) results)))
                            init-envs)))
              (dolist (pp (nreverse path-pats))
              (setf envs (apply-path-pattern g envs pp)))
            (dolist (sq (nreverse subquery-pats))
              (setf envs (apply-subquery-pattern g envs sq)))
            (dolist (sp (nreverse service-pats))
              (setf envs (apply-service-pattern envs sp)))
            ;; Apply inline EXISTS/NOT-EXISTS/GRAPH
            (dolist (p patterns)
              (when (and (consp p) (symbolp (car p)))
                (cond
                  ((sym-name-equal (car p) "EXISTS")
                   (setf envs (remove-if-not
                               (lambda (env)
                                 (match-patterns-with-envs g (rest p) (list env)))
                               envs)))
                  ((sym-name-equal (car p) "NOT-EXISTS")
                   (setf envs (apply-not-exists g envs (rest p))))
                  ((sym-name-equal (car p) "GRAPH")
                   (setf envs (match-graph-pattern g envs p))))))
            envs))))))

(defun apply-values-inline (envs clause)
  "Apply inline VALUES: inject bindings into environments."
  (let ((var-spec (first clause))
        (data (second clause))
        (results nil))
    (dolist (env envs)
      (dolist (row data)
        (if (variable-p var-spec)
            ;; Single variable
            (let ((val (first row)))
              (when (or (null val)
                        (let ((existing (lookup-binding var-spec env)))
                          (or (null existing) (equal existing val))))
                (push (if val (cons (cons var-spec val) env) env) results)))
            ;; Multiple variables
            (let ((new-env env) (ok t))
              (mapc (lambda (var val)
                      (when val
                        (let ((existing (lookup-binding var new-env)))
                          (cond ((null existing)
                                 (push (cons var val) new-env))
                                ((not (equal existing val))
                                 (setf ok nil))))))
                    var-spec row)
              (when ok (push new-env results))))))
    (nreverse results)))

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
  "Match any predicate EXCEPT the excluded one(s). Exclusions can be strings or (INV string)."
  (let* ((bound-start (and (not (variable-p start)) start))
         (excluded (if (and (listp excluded-pred) (not (sym-name-equal (car excluded-pred) "INV")))
                       excluded-pred (list excluded-pred)))
         (direct-excl (remove-if #'consp excluded))
         (inverse-excl (mapcar #'second (remove-if-not #'consp excluded)))
         (results nil))
    ;; Direct matches (only if there are direct exclusions, or no inverse exclusions)
    (when direct-excl
      (if bound-start
          (dolist (tr (get-triples g :subject bound-start))
            (unless (member (triple-predicate tr) direct-excl :test #'equal)
              (push (cons bound-start (triple-object tr)) results)))
          (dolist (subj (all-subjects g))
            (dolist (tr (get-triples g :subject subj))
              (unless (member (triple-predicate tr) direct-excl :test #'equal)
                (push (cons subj (triple-object tr)) results))))))
    ;; Inverse matches (only if there are inverse exclusions)
    (when inverse-excl
      (if bound-start
          (dolist (tr (get-triples g :object bound-start))
            (unless (member (triple-predicate tr) inverse-excl :test #'equal)
              (push (cons bound-start (triple-subject tr)) results)))
          (dolist (tr (get-triples g))
            (unless (member (triple-predicate tr) inverse-excl :test #'equal)
              (push (cons (triple-object tr) (triple-subject tr)) results)))))
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
        (bound-target (and target (not (variable-p target)) target))
        (results nil))
    (when bound-start
      (pushnew (cons bound-start bound-start) results :test #'equal)
      (dolist (next (one-hop g bound-start pred))
        (pushnew (cons bound-start next) results :test #'equal)))
    (when (and bound-target (not bound-start))
      ;; Unbound start, bound target: target matches itself + inverse one-hop
      (pushnew (cons bound-target bound-target) results :test #'equal)
      (dolist (tr (get-triples g :object bound-target))
        (when (equal (triple-predicate tr) (if (stringp pred) pred (lit-val pred)))
          (pushnew (cons (triple-subject tr) bound-target) results :test #'equal))))
    (if bound-target
        (remove-if-not (lambda (pair) (equal (cdr pair) bound-target)) results)
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
    (if bound-start
        (progn
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
        ;; Unbound start — every node can reach itself + transitive targets
        (let ((all-nodes (all-nodes g)))
          (dolist (node all-nodes)
            (push (cons node node) results)
            (let ((visited (make-hash-table :test 'equal)))
              (setf (gethash node visited) t)
              (labels ((walk (n)
                         (dolist (next (one-hop g n pred))
                           (unless (gethash next visited)
                             (setf (gethash next visited) t)
                             (push (cons node next) results)
                             (walk next)))))
                (walk node))))))
    ;; For zero-length path: bound terms always match themselves
    (when (and target (not (variable-p target)))
      (pushnew (cons target target) results :test #'equal))
    (when (and bound-start (not (variable-p start)))
      (pushnew (cons bound-start bound-start) results :test #'equal))
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
                            (or (null val)  ; UNDEF matches anything
                                (equal (lookup-binding var env) val)))
                          var-spec row))
                 data))
         envs))))

;;; ==========================================================================
;;; Subqueries
;;; ==========================================================================

(defun apply-subquery-pattern (g envs pattern)
  "Apply a subquery pattern: (subquery <query-expr>) or (subquery <query-expr> <bind-var>)."
  (let* ((sq-expr (second pattern))
         (bind-var (third pattern))
         (sq-results (query g sq-expr)))
    (if bind-var
        ;; Old format: filter envs by bind-var membership in subquery results
        (let ((sq-values (mapcar (lambda (row)
                                   (if (= 1 (length row)) (first row) row))
                                 sq-results)))
          (remove-if-not
           (lambda (env)
             (member (lookup-binding bind-var env) sq-values :test #'equal))
           envs))
        ;; New format: join subquery results with outer environments
        (let ((sq-vars (when (and (listp sq-expr) (listp (second sq-expr)))
                         (remove-if-not #'variable-p (second sq-expr))))
              (results nil))
          (dolist (env envs)
            (dolist (row sq-results)
              (let ((new-env env) (ok t))
                (loop for var in sq-vars for val in row do
                  (let ((existing (assoc var new-env)))
                    (cond ((null existing) (push (cons var val) new-env))
                          ((equal (lit-val (cdr existing)) (lit-val val)))
                          (t (setf ok nil)))))
                (when ok (push new-env results)))))
          (nreverse results)))))

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

(defun match-graph-pattern (g envs graph-pat)
  "Handle (GRAPH uri patterns...) by matching in the named graph."
  (let ((graph-name (second graph-pat))
        (inner-patterns (cddr graph-pat)))
    (let ((results nil))
      (dolist (env envs)
        (let* ((resolved-name (if (variable-p graph-name)
                                  (or (lookup-binding graph-name env) graph-name)
                                  graph-name))
               (new-envs (match-in-graph g inner-patterns resolved-name)))
          (dolist (ne new-envs)
            (push (append ne env) results))))
      (nreverse results))))

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
