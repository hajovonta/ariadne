;;;; sparql-parser.lisp
;;;; Parse SPARQL query strings into Ariadne DSL and execute

(in-package #:ariadne)

(defun sparql (g query-string)
  "Parse and execute a SPARQL query string against graph G."
  (let ((expr (parse-sparql query-string)))
    (query g expr)))

(defun parse-sparql (str)
  "Parse a SPARQL query string into an Ariadne DSL expression."
  (let ((tokens (sparql-tokenize str))
        (prefixes (make-hash-table :test 'equal)))
    (sparql-parse-query tokens prefixes)))

;;; ==========================================================================
;;; Tokenizer
;;; ==========================================================================

(defun sparql-tokenize (str)
  "Tokenize a SPARQL query string."
  (let ((tokens nil)
        (pos 0)
        (len (length str)))
    (flet ((skip-ws ()
             (loop while (and (< pos len)
                              (member (char str pos) '(#\Space #\Tab #\Newline #\Return)))
                   do (incf pos))))
      (loop while (< pos len) do
        (skip-ws)
        (when (< pos len)
          (let ((ch (char str pos)))
            (cond
              ;; Punctuation
              ((member ch '(#\{ #\} #\( #\) #\. #\+ #\* #\^))
               (push (string ch) tokens)
               (incf pos))
              ;; Variable ?name
              ((char= ch #\?)
               (let ((start pos))
                 (incf pos)
                 (loop while (and (< pos len)
                                  (alphanumericp (char str pos)))
                       do (incf pos))
                 (push (intern (string-upcase (subseq str start pos))) tokens)))
              ;; URI <...>
              ;; URI <...> or comparison operator <
              ((char= ch #\<)
               (if (and (< (1+ pos) len)
                        (or (alpha-char-p (char str (1+ pos)))
                            (char= #\/ (char str (1+ pos)))))
                   ;; URI
                   (let ((end (position #\> str :start (1+ pos))))
                     (when end
                       (push (subseq str (1+ pos) end) tokens)
                       (setf pos (1+ end))))
                   ;; Comparison operator
                   (let ((start pos))
                     (incf pos)
                     (when (and (< pos len) (char= #\= (char str pos)))
                       (incf pos))
                     (push (intern (subseq str start pos)) tokens))))
              ;; String "..."
              ((char= ch #\")
               (let ((start (1+ pos)))
                 (incf pos)
                 (loop while (and (< pos len) (char/= #\" (char str pos)))
                       do (when (char= #\\ (char str pos)) (incf pos))
                          (incf pos))
                 (push (subseq str start pos) tokens)
                 (when (< pos len) (incf pos))))
              ;; Number
              ((or (digit-char-p ch)
                   (and (char= ch #\-) (< (1+ pos) len) (digit-char-p (char str (1+ pos)))))
               (let ((start pos))
                 (incf pos)
                 (loop while (and (< pos len)
                                  (or (digit-char-p (char str pos))
                                      (char= #\. (char str pos))))
                       do (incf pos))
                 (push (read-from-string (subseq str start pos)) tokens)))
              ;; Comparison operators (>, >=, =, !=, !)
              ((member ch '(#\> #\= #\!))
               (let ((start pos))
                 (incf pos)
                 (when (and (< pos len) (char= #\= (char str pos)))
                   (incf pos))
                 (push (intern (subseq str start pos)) tokens)))
              ((char= ch #\&)
               (incf pos)
               (when (and (< pos len) (char= #\& (char str pos)))
                 (incf pos))
               (push "&&" tokens))
              ((char= ch #\|)
               (incf pos)
               (when (and (< pos len) (char= #\| (char str pos)))
                 (incf pos))
               (push "||" tokens))
              ((char= ch #\,)
               (push "," tokens)
               (incf pos))
              ;; Keyword or prefixed name
              (t
               (let ((start pos))
                 (loop while (and (< pos len)
                                  (not (member (char str pos)
                                               '(#\Space #\Tab #\Newline #\Return
                                                 #\{ #\} #\( #\) #\. #\;))))
                       do (incf pos))
                 (let ((tok (subseq str start pos)))
                   (push tok tokens)))))))))
    (nreverse tokens)))

;;; ==========================================================================
;;; Parser
;;; ==========================================================================

(defun sparql-parse-query (tokens prefixes)
  "Parse tokens into an Ariadne DSL expression."
  (let ((toks tokens))
    ;; Parse PREFIX declarations
    (loop while (and toks (string-equal (car toks) "PREFIX")) do
      (pop toks) ; PREFIX
      (let ((prefix-name (pop toks))  ; "ex:"
            (uri (pop toks)))         ; full URI
        (setf (gethash prefix-name prefixes) uri)))
    ;; Parse query form
    (let ((form (pop toks)))
      (cond
        ((string-equal form "SELECT")
         (sparql-parse-select toks prefixes nil))
        ((string-equal form "ASK")
         (sparql-parse-ask toks prefixes))
        ((string-equal form "CONSTRUCT")
         (sparql-parse-construct toks prefixes))
        ((string-equal form "DESCRIBE")
         (sparql-parse-describe toks prefixes))
        (t (error "Unknown SPARQL query form: ~A" form))))))

(defun sparql-parse-select (toks prefixes distinct-p)
  "Parse SELECT query."
  ;; Check for DISTINCT
  (when (and toks (string-equal (car toks) "DISTINCT"))
    (pop toks)
    (setf distinct-p t))
  ;; Parse variable list (including aggregations like (COUNT ?var))
  (let ((vars nil))
    (loop while (and toks
                     (not (and (stringp (car toks)) (string-equal (car toks) "WHERE")))
                     (not (string= (car toks) "{")))
          do (if (string= (car toks) "(")
                 ;; Aggregation: (COUNT ?var)
                 (progn
                   (pop toks)
                   (let ((fn (intern (string-upcase (princ-to-string (pop toks)))))
                         (var (pop toks)))
                     (when (and toks (string= (car toks) ")"))
                       (pop toks))
                     (push (list fn var) vars)))
                 (push (pop toks) vars)))
    (setf vars (nreverse vars))
    ;; Expect WHERE
    (when (and toks (string-equal (car toks) "WHERE"))
      (pop toks))
    ;; Expect {
    (when (and toks (string= (car toks) "{"))
      (pop toks))
    ;; Parse patterns and filters
    (multiple-value-bind (patterns filters toks-rest optionals unions binds)
        (sparql-parse-body toks prefixes)
      (setf toks toks-rest)
      ;; Expect }
      (when (and toks (string= (car toks) "}"))
        (pop toks))
      ;; Parse trailing clauses
      (let ((clauses nil))
        (loop while toks do
          (cond
            ((string-equal (car toks) "LIMIT")
             (pop toks)
             (push (list 'limit (pop toks)) clauses))
            ((string-equal (car toks) "OFFSET")
             (pop toks)
             (push (list 'offset (pop toks)) clauses))
            ((string-equal (car toks) "ORDER")
             (pop toks)
             (when (and toks (string-equal (car toks) "BY"))
               (pop toks))
             (push (list 'order-by (pop toks)) clauses))
            ((string-equal (car toks) "GROUP")
             (pop toks)
             (when (and toks (string-equal (car toks) "BY"))
               (pop toks))
             (push (list 'group-by (pop toks)) clauses))
            ((string-equal (car toks) "HAVING")
             (pop toks)
             ;; Parse HAVING expression: (agg ?var) op value
             (let ((agg-or-paren (pop toks))
                   having-expr)
               (if (string= agg-or-paren "(")
                   ;; (COUNT ?var) > N
                   (let ((agg-fn (intern (string-upcase (princ-to-string (pop toks)))))
                         (agg-var (pop toks)))
                     (pop toks) ; )
                     (let ((op (intern (string-upcase (princ-to-string (pop toks)))))
                           (val (sparql-resolve-term (pop toks) prefixes)))
                       (setf having-expr (list op (list agg-fn agg-var) val))))
                   (setf having-expr agg-or-paren))
               (push (list 'having having-expr) clauses)))
            (t (return))))
        ;; Build DSL expression
        (let ((expr (list (if distinct-p 'select-distinct 'select)
                          vars
                          (cons 'where (remove-if (lambda (p) (and (consp p) (eq (car p) 'not-exists))) patterns)))))
          ;; Extract NOT-EXISTS from patterns
          (dolist (p patterns)
            (when (and (consp p) (eq (car p) 'not-exists))
              (setf expr (append expr (list p)))))
          (dolist (opt optionals)
            (setf expr (append expr (list opt))))
          (dolist (u unions)
            (setf expr (append expr (list u))))
          (when filters
            (setf expr (append expr (list (cons 'filter filters)))))
          (dolist (b binds)
            (setf expr (append expr (list b))))
          (dolist (c clauses)
            (setf expr (append expr (list c))))
          expr)))))

(defun sparql-parse-ask (toks prefixes)
  "Parse ASK query."
  (when (and toks (string= (car toks) "{"))
    (pop toks))
  (multiple-value-bind (patterns filters toks-rest optionals unions binds)
      (sparql-parse-body toks prefixes)
    (declare (ignore toks-rest optionals unions))
    (let ((expr (list 'ask (cons 'where patterns))))
      (when filters (nconc expr (list (cons 'filter filters))))
      (dolist (b binds) (nconc expr (list (cons 'bind b))))
      expr)))

;;; ==========================================================================
;;; FILTER expression parser
;;; ==========================================================================

(defun parse-sparql-filter-expr (toks prefixes)
  "Parse a FILTER expression. Returns (values expr remaining-toks)."
  ;; Consume opening paren if present
  (when (and toks (string= (car toks) "("))
    (pop toks))
  (multiple-value-bind (expr rest) (parse-or-expr toks prefixes)
    ;; Consume closing paren if present
    (when (and rest (stringp (car rest)) (string= (car rest) ")"))
      (pop rest))
    (values expr rest)))

(defun parse-or-expr (toks prefixes)
  "Parse OR expression: expr || expr"
  (multiple-value-bind (left rest) (parse-and-expr toks prefixes)
    (loop while (and rest (stringp (car rest)) (string= (car rest) "||")) do
      (pop rest)
      (multiple-value-bind (right rest2) (parse-and-expr rest prefixes)
        (setf left (list 'or left right))
        (setf rest rest2)))
    (values left rest)))

(defun parse-and-expr (toks prefixes)
  "Parse AND expression: expr && expr"
  (multiple-value-bind (left rest) (parse-compare-expr toks prefixes)
    (loop while (and rest (stringp (car rest)) (string= (car rest) "&&")) do
      (pop rest)
      (multiple-value-bind (right rest2) (parse-compare-expr rest prefixes)
        (setf left (list 'and left right))
        (setf rest rest2)))
    (values left rest)))

(defun parse-compare-expr (toks prefixes)
  "Parse comparison: expr op expr"
  (multiple-value-bind (left rest) (parse-unary-expr toks prefixes)
    (when (and rest (symbolp (car rest))
                (member (car rest) '(= != < > <= >=) :test #'eq))
      (let ((op (pop rest)))
        (multiple-value-bind (right rest2) (parse-unary-expr rest prefixes)
          (setf left (list op left right))
          (setf rest rest2))))
    (values left rest)))

(defun parse-unary-expr (toks prefixes)
  "Parse unary: !expr or primary"
  (if (and toks (symbolp (car toks)) (string= (symbol-name (car toks)) "!"))
      (progn
        (pop toks)
        (multiple-value-bind (expr rest) (parse-primary-expr toks prefixes)
          (values (list 'not expr) rest)))
      (parse-primary-expr toks prefixes)))

(defun parse-primary-expr (toks prefixes)
  "Parse primary: (expr), function(args), variable, literal, URI, true, false"
  (cond
    ((null toks) (values nil nil))
    ;; Parenthesized expression
    ((and (stringp (car toks)) (string= (car toks) "("))
     (pop toks)
     (multiple-value-bind (expr rest) (parse-or-expr toks prefixes)
       (when (and rest (stringp (car rest)) (string= (car rest) ")"))
         (pop rest))
       (values expr rest)))
    ;; Boolean constants
    ((and (stringp (car toks)) (string-equal (car toks) "true"))
     (pop toks) (values t toks))
    ((and (stringp (car toks)) (string-equal (car toks) "false"))
     (pop toks) (values '(not t) toks))
    ;; Function call: name(args)
    ((and (stringp (car toks))
          (cdr toks)
          (stringp (cadr toks))
          (string= (cadr toks) "(")
          (alpha-char-p (char (car toks) 0)))
     (let ((fname (string-upcase (pop toks))))
       (pop toks) ; consume (
       (let ((args nil))
         (loop until (or (null toks) (and (stringp (car toks)) (string= (car toks) ")"))) do
           (multiple-value-bind (arg rest) (parse-or-expr toks prefixes)
             (push arg args)
             (setf toks rest))
           (when (and toks (stringp (car toks)) (string= (car toks) ","))
             (pop toks)))
         (when (and toks (stringp (car toks)) (string= (car toks) ")"))
           (pop toks))
         (values (cons (intern fname) (nreverse args)) toks))))
    ;; Regular term (variable, URI, literal, number)
    (t (values (sparql-resolve-term (pop toks) prefixes) toks))))

(defun sparql-parse-body (toks prefixes)
  "Parse the body of a WHERE clause. Returns (values patterns filters remaining-toks optionals unions)."
  (let ((patterns nil)
        (filters nil)
        (optionals nil)
        (unions nil)
        (binds nil))
    (loop while (and toks (not (string= (car toks) "}"))) do
      (cond
        ;; FILTER
        ((string-equal (car toks) "FILTER")
         (pop toks)
         ;; FILTER NOT EXISTS { ... }
         (if (and toks (stringp (car toks)) (string-equal (car toks) "NOT")
                  (cdr toks) (stringp (cadr toks)) (string-equal (cadr toks) "EXISTS"))
             (progn
               (pop toks) (pop toks)
               (when (and toks (stringp (car toks)) (string= (car toks) "{"))
                 (pop toks))
               (multiple-value-bind (ne-pats ne-filts ne-rest)
                   (sparql-parse-body toks prefixes)
                 (declare (ignore ne-filts))
                 (setf toks ne-rest)
                 (when (and toks (stringp (car toks)) (string= (car toks) "}"))
                   (pop toks))
                 (push (cons 'not-exists ne-pats) patterns)))
             ;; Regular FILTER expression
             (multiple-value-bind (expr rest) (parse-sparql-filter-expr toks prefixes)
               (setf toks rest)
               (when expr (push expr filters))))
         (when (and toks (stringp (car toks)) (string= (car toks) "."))
           (pop toks)))
        ;; BIND (expr AS ?var)
        ((string-equal (car toks) "BIND")
         (pop toks)
         (when (and toks (stringp (car toks)) (string= (car toks) "("))
           (pop toks))
         ;; Read expression (may be a URI, variable, or complex expr)
         (let* ((left (sparql-resolve-term (pop toks) prefixes))
                (op (when (and toks
                               (not (and (stringp (car toks)) (string= (car toks) ")")))
                               (not (and (stringp (car toks)) (string-equal (car toks) "AS"))))
                      (intern (string-upcase (princ-to-string (pop toks))))))
                (right (when op (sparql-resolve-term (pop toks) prefixes)))
                (expr (if op (list op left right) left)))
           (when (and toks (stringp (car toks)) (string-equal (car toks) "AS"))
             (pop toks))
           (let ((var (pop toks)))
             (push (list 'bind var expr) binds))
           (when (and toks (stringp (car toks)) (string= (car toks) ")"))
             (pop toks))
           (when (and toks (stringp (car toks)) (string= (car toks) "."))
             (pop toks))))
        ;; SERVICE <url> { ... }
        ((string-equal (car toks) "SERVICE")
         (pop toks)
         (let ((url (sparql-resolve-term (pop toks) prefixes)))
           (when (and toks (string= (car toks) "{"))
             (pop toks))
           (multiple-value-bind (svc-patterns svc-filters svc-rest)
               (sparql-parse-body toks prefixes)
             (declare (ignore svc-filters))
             (setf toks svc-rest)
             (when (and toks (string= (car toks) "}"))
               (pop toks))
             (push (list 'service url svc-patterns) patterns))))
        ;; OPTIONAL { ... }
        ((string-equal (car toks) "OPTIONAL")
         (pop toks)
         (when (and toks (string= (car toks) "{"))
           (pop toks))
         (multiple-value-bind (opt-patterns opt-filters opt-rest)
             (sparql-parse-body toks prefixes)
           (declare (ignore opt-filters))
           (setf toks opt-rest)
           (when (and toks (string= (car toks) "}"))
             (pop toks))
           (push (cons 'optional opt-patterns) optionals)))
        ;; UNION or nested block: { ... } UNION { ... } or { SELECT subquery }
        ((string= (car toks) "{")
         (pop toks)
         ;; Check for subquery: { SELECT ... WHERE { ... } }
         (if (and toks (stringp (car toks)) (string-equal (car toks) "SELECT"))
             (progn
               ;; Skip SELECT and variables until WHERE
               (loop while (and toks (not (and (stringp (car toks)) (string-equal (car toks) "WHERE"))))
                     do (pop toks))
               (when (and toks (stringp (car toks)) (string-equal (car toks) "WHERE"))
                 (pop toks))
               (when (and toks (stringp (car toks)) (string= (car toks) "{"))
                 (pop toks))
               ;; Parse inner body — merge patterns/filters into outer scope
               (multiple-value-bind (sub-pats sub-filts sub-rest sub-opts sub-unions sub-binds)
                   (sparql-parse-body toks prefixes)
                 (dolist (p sub-pats) (push p patterns))
                 (dolist (f sub-filts) (push f filters))
                 (dolist (b sub-binds) (push b binds))
                 (setf toks sub-rest))
               ;; Skip inner and outer closing braces
               (when (and toks (stringp (car toks)) (string= (car toks) "}"))
                 (pop toks))
               (when (and toks (stringp (car toks)) (string= (car toks) "}"))
                 (pop toks)))
             ;; Regular nested block or UNION
             (multiple-value-bind (u-patterns u-filters u-rest)
             (sparql-parse-body toks prefixes)
           (declare (ignore u-filters))
           (setf toks u-rest)
           (when (and toks (string= (car toks) "}"))
             (pop toks))
           (if (and toks (string-equal (car toks) "UNION"))
               (progn
                 (pop toks)
                 (when (and toks (string= (car toks) "{"))
                   (pop toks))
                 (multiple-value-bind (u2-patterns u2-filters u2-rest)
                     (sparql-parse-body toks prefixes)
                   (declare (ignore u2-filters))
                   (setf toks u2-rest)
                   (when (and toks (string= (car toks) "}"))
                     (pop toks))
                   (push (list 'union
                               (cons 'where u-patterns)
                               (cons 'where u2-patterns))
                         unions)))
               ;; Not UNION, just nested block — treat as patterns
               (dolist (p u-patterns) (push p patterns))))))
        ;; Triple pattern: s p o .  (with property path detection)
        (t
         (let* ((s (sparql-resolve-term (pop toks) prefixes))
                ;; Check for inverse path: ^pred
                (inverse-p (when (and toks (stringp (car toks)) (string= (car toks) "^"))
                             (pop toks) t))
                (p-raw (pop toks))
                ;; Check for path modifier suffix: + or *
                (modifier (when (and toks (stringp (car toks))
                                     (member (car toks) '("+" "*") :test #'string=))
                            (pop toks)))
                (p-resolved (sparql-resolve-term p-raw prefixes))
                (p (cond
                     ((and inverse-p modifier (string= modifier "+"))
                      (list 'inv+ p-resolved))
                     (inverse-p (list 'inv p-resolved))
                     ((and modifier (string= modifier "+"))
                      (list '+ p-resolved))
                     ((and modifier (string= modifier "*"))
                      (list '* p-resolved))
                     (t p-resolved)))
                (o (sparql-resolve-term (pop toks) prefixes)))
           (push (list s p o) patterns))
         (when (and toks (string= (car toks) "."))
           (pop toks)))))
    (values (nreverse patterns) (nreverse filters) toks (nreverse optionals) (nreverse unions) (nreverse binds))))

(defun sparql-resolve-term (term prefixes)
  "Resolve a SPARQL term: expand prefixed names, keep variables as symbols."
  (cond
    ((null term) nil)
    ((symbolp term) term)  ; ?variable
    ((numberp term) term)
    ((stringp term)
     ;; Check for prefixed name (contains : but not ://)
     (let ((colon (position #\: term)))
       (if (and colon (not (search "://" term)))
           (let* ((prefix (concatenate 'string (subseq term 0 (1+ colon))))
                  (local (subseq term (1+ colon)))
                  (base (gethash prefix prefixes)))
             (if base
                 (concatenate 'string base local)
                 term))
           term)))
    (t term)))


(defun sparql-parse-construct (toks prefixes)
  "Parse CONSTRUCT { template } WHERE { patterns }."
  ;; Parse template { s p o }
  (when (and toks (string= (car toks) "{"))
    (pop toks))
  (let ((template nil))
    (loop while (and toks (not (string= (car toks) "}"))) do
      (let ((s (sparql-resolve-term (pop toks) prefixes))
            (p (sparql-resolve-term (pop toks) prefixes))
            (o (sparql-resolve-term (pop toks) prefixes)))
        (push (list s p o) template))
      (when (and toks (string= (car toks) "."))
        (pop toks)))
    (when (and toks (string= (car toks) "}"))
      (pop toks))
    ;; Parse WHERE
    (when (and toks (string-equal (car toks) "WHERE"))
      (pop toks))
    (when (and toks (string= (car toks) "{"))
      (pop toks))
    (multiple-value-bind (patterns filters toks-rest)
        (sparql-parse-body toks prefixes)
      (declare (ignore filters toks-rest))
      (when (and toks (string= (car toks) "}"))
        (pop toks))
      (list 'construct (first (nreverse template)) (cons 'where patterns)))))

(defun sparql-parse-describe (toks prefixes)
  "Parse DESCRIBE <resource>."
  (let ((resource (sparql-resolve-term (pop toks) prefixes)))
    (list 'describe resource)))

;;; ==========================================================================
;;; SPARQL UPDATE
;;; ==========================================================================

(defun sparql-update (g update-string)
  "Execute a SPARQL UPDATE string (INSERT DATA / DELETE DATA)."
  (let ((prefixes (make-hash-table :test 'equal))
        (s (string-trim '(#\Space #\Tab #\Newline #\Return) update-string)))
    ;; Parse PREFIX declarations
    (loop while (and (> (length s) 7)
                     (string-equal "PREFIX" (subseq s 0 6)))
          do (let* ((rest (string-trim '(#\Space #\Tab) (subseq s 6)))
                    (colon (position #\: rest))
                    (prefix (subseq rest 0 (1+ colon)))
                    (after (string-trim '(#\Space #\Tab) (subseq rest (1+ colon))))
                    (uri-end (position #\> after))
                    (uri (subseq after 1 uri-end)))
               (setf (gethash prefix prefixes) uri)
               (setf s (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    (subseq after (1+ uri-end))))))
    ;; Determine operation
    (cond
      ((and (>= (length s) 11) (string-equal "INSERT DATA" (subseq s 0 11)))
       (let ((triples (parse-update-triples (subseq s 11) prefixes)))
         (dolist (tr triples)
           (add-triple g (first tr) (second tr) (third tr)))
         (length triples)))
      ((and (>= (length s) 11) (string-equal "DELETE DATA" (subseq s 0 11)))
       (let ((triples (parse-update-triples (subseq s 11) prefixes)))
         (dolist (tr triples)
           (remove-triple g (first tr) (second tr) (third tr)))
         (length triples)))
      ((and (>= (length s) 12) (string-equal "DELETE WHERE" (subseq s 0 12)))
       (let* ((body (subseq s 12))
              (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) body))
              (inner (if (and (> (length trimmed) 1)
                              (char= #\{ (char trimmed 0)))
                         (subseq trimmed 1 (position #\} trimmed :from-end t))
                         trimmed))
              (tokens (sparql-tokenize inner))
              (count 0))
         ;; Parse pattern triples, keeping ?vars as-is
         (let ((patterns nil))
           (loop while (>= (length tokens) 3) do
             (let ((s-tok (pop tokens))
                   (p-tok (pop tokens))
                   (o-tok (pop tokens)))
               (flet ((to-pat (tok)
                        (if (and (symbolp tok)
                                 (char= #\? (char (symbol-name tok) 0)))
                            tok
                            (resolve-sparql-token tok prefixes))))
                 (push (list (to-pat s-tok) (to-pat p-tok) (to-pat o-tok)) patterns)))
             (when (and tokens (stringp (car tokens)) (string= "." (car tokens)))
               (pop tokens)))
           (dolist (pat (nreverse patterns))
             (let ((matches (match-pattern g pat)))
               (dolist (bindings matches)
                 (let ((ms (if (variable-p (first pat))
                               (cdr (assoc (first pat) bindings))
                               (first pat)))
                       (mp (if (variable-p (second pat))
                               (cdr (assoc (second pat) bindings))
                               (second pat)))
                       (mo (if (variable-p (third pat))
                               (cdr (assoc (third pat) bindings))
                               (third pat))))
                   (when (remove-triple g ms mp mo)
                     (incf count)))))))
         count))
      (t (error "Unsupported SPARQL UPDATE operation")))))

(defun parse-update-triples (body prefixes)
  "Parse { s p o . s p o . } into list of (s p o) lists."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) body))
         (inner (if (and (> (length trimmed) 1)
                         (char= #\{ (char trimmed 0)))
                    (subseq trimmed 1 (position #\} trimmed :from-end t))
                    trimmed))
         (tokens (sparql-tokenize inner))
         (result nil))
    (loop while (>= (length tokens) 3) do
      (let ((s (resolve-sparql-token (pop tokens) prefixes))
            (p (resolve-sparql-token (pop tokens) prefixes))
            (o (resolve-sparql-token (pop tokens) prefixes)))
        (push (list s p o) result)
        ;; Skip optional dot
        (when (and tokens (string= "." (car tokens)))
          (pop tokens))))
    (nreverse result)))

(defun resolve-sparql-token (tok prefixes)
  "Resolve a SPARQL token using prefixes."
  (cond
    ((and (> (length tok) 1) (char= #\< (char tok 0)) (char= #\> (char tok (1- (length tok)))))
     (subseq tok 1 (1- (length tok))))
    ((and (> (length tok) 1) (char= #\" (char tok 0)))
     (subseq tok 1 (position #\" tok :start 1)))
    ((position #\: tok)
     (let* ((colon (position #\: tok))
            (prefix (subseq tok 0 (1+ colon)))
            (local (subseq tok (1+ colon)))
            (base (gethash prefix prefixes)))
       (if base (concatenate 'string base local) tok)))
    (t tok)))
