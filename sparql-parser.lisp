;;;; sparql-parser.lisp
;;;; Parse SPARQL query strings into Ariadne DSL and execute

(in-package #:ariadne)

(defvar *sparql-anon-counter* 0)

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
             (loop
               (loop while (and (< pos len)
                                (member (char str pos) '(#\Space #\Tab #\Newline #\Return)))
                     do (incf pos))
               (if (and (< pos len) (char= (char str pos) #\#))
                   (loop while (and (< pos len) (char/= (char str pos) #\Newline))
                         do (incf pos))
                   (return)))))
      (loop while (< pos len) do
        (skip-ws)
        (when (< pos len)
          (let ((ch (char str pos)))
            (cond
              ;; Punctuation
              ((member ch '(#\{ #\} #\( #\) #\. #\+ #\* #\^ #\;))
               (push (string ch) tokens)
               (incf pos))
              ;; | or || 
              ((char= ch #\|)
               (if (and (< (1+ pos) len) (char= #\| (char str (1+ pos))))
                   (progn (push "||" tokens) (incf pos 2))
                   (progn (push "|" tokens) (incf pos))))
              ;; /
              ((char= ch #\/)
               (push "/" tokens)
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
              ;; String "..." possibly with ^^type or @lang
              ((char= ch #\")
               (let ((start pos))
                 (incf pos)
                 (loop while (and (< pos len) (char/= #\" (char str pos)))
                       do (when (char= #\\ (char str pos)) (incf pos))
                          (incf pos))
                 (when (< pos len) (incf pos)) ; skip closing quote
                 ;; Consume ^^type or @lang suffix
                 (cond
                   ((and (<= (+ pos 2) len) (char= #\^ (char str pos)) (char= #\^ (char str (1+ pos))))
                    (incf pos 2)
                    (if (and (< pos len) (char= #\< (char str pos)))
                        (let ((end (position #\> str :start pos)))
                          (when end (setf pos (1+ end))))
                        (loop while (and (< pos len)
                                         (not (member (char str pos) '(#\Space #\Tab #\Newline #\Return #\. #\) #\} #\;))))
                              do (incf pos))))
                   ((and (< pos len) (char= #\@ (char str pos)))
                    (loop while (and (< pos len)
                                     (not (member (char str pos) '(#\Space #\Tab #\Newline #\Return #\. #\) #\} #\;))))
                          do (incf pos))))
                 (push (subseq str start pos) tokens)))
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
              ((char= ch #\,)
               (push "," tokens)
               (incf pos))
              ;; Keyword or prefixed name
              (t
               (let ((start pos))
                 (loop while (and (< pos len)
                                  (not (member (char str pos)
                                               '(#\Space #\Tab #\Newline #\Return
                                                 #\{ #\} #\( #\) #\. #\; #\| #\/ #\* #\+ #\^ #\!
                                                 #\, #\= #\< #\>))))
                       do (incf pos))
                 (let ((tok (subseq str start pos)))
                   (push tok tokens)))))))))
    (nreverse tokens)))

;;; ==========================================================================
;;; Parser
;;; ==========================================================================

(defun sparql-parse-query (tokens prefixes)
  "Parse tokens into an Ariadne DSL expression."
  (let ((toks tokens)
        (base-uri nil))
    ;; Parse BASE and PREFIX declarations
    (loop while (and toks (or (string-equal (car toks) "PREFIX")
                              (string-equal (car toks) "BASE"))) do
      (cond
        ((string-equal (car toks) "BASE")
         (pop toks)
         (setf base-uri (pop toks))
         (setf (gethash "" prefixes) base-uri))
        ((string-equal (car toks) "PREFIX")
         (pop toks)
         (let ((prefix-name (pop toks))
               (uri (pop toks)))
           (setf (gethash prefix-name prefixes) uri)))))
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
  ;; Parse variable list (including expressions like (COUNT(?var) AS ?alias))
  (let ((vars nil)
        (projections nil))  ; list of (alias . expr) for computed columns
    (loop while (and toks
                     (not (and (stringp (car toks)) (string-equal (car toks) "WHERE")))
                     (not (and (stringp (car toks)) (string= (car toks) "{"))))
          do (if (and (stringp (car toks)) (string= (car toks) "("))
                 ;; Expression: (expr AS ?var) or (AGG ?var)
                 (let ((depth 1) (expr-toks nil))
                   (pop toks) ; consume (
                   ;; Collect tokens until matching )
                   (loop while (and toks (> depth 0)) do
                     (cond ((and (stringp (car toks)) (string= (car toks) "(")) (incf depth))
                           ((and (stringp (car toks)) (string= (car toks) ")")) (decf depth)))
                     (when (> depth 0) (push (pop toks) expr-toks))
                     (when (= depth 0) (pop toks))) ; consume final )
                   (setf expr-toks (nreverse expr-toks))
                   ;; Find AS — split into expr and alias
                   (let ((as-pos (position "AS" expr-toks :test #'string-equal :key (lambda (x) (if (stringp x) x "")))))
                     (if as-pos
                         (let ((expr-part (subseq expr-toks 0 as-pos))
                               (alias (nth (1+ as-pos) expr-toks)))
                           (push alias vars)
                           (push (cons alias expr-part) projections))
                         ;; No AS — old style (COUNT ?var)
                         (let ((fn (intern (string-upcase (princ-to-string (first expr-toks)))))
                               (var (second expr-toks)))
                           (push (list fn var) vars)))))
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
             ;; Parse HAVING expressions — each parenthesized condition
             (let ((exprs nil))
               (loop while (and toks (stringp (car toks)) (string= (car toks) "(")) do
                 (multiple-value-bind (expr rest) (parse-or-expr toks prefixes)
                   (setf toks rest)
                   (when expr (push expr exprs))))
               (push (list 'having (nreverse exprs)) clauses)))
            ((string-equal (car toks) "VALUES")
             (pop toks)
             ;; VALUES ?var { val1 val2 ... } or VALUES (?v1 ?v2) { (v1 v2) ... }
             (let ((val-vars nil) (val-data nil))
               (if (and toks (stringp (car toks)) (string= (car toks) "("))
                   ;; Multi-variable: VALUES (?v1 ?v2) { ... }
                   (progn
                     (pop toks)
                     (loop until (or (null toks) (string= (car toks) ")")) do
                       (push (pop toks) val-vars))
                     (when toks (pop toks))
                     (setf val-vars (nreverse val-vars)))
                   ;; Single variable
                   (push (pop toks) val-vars))
               ;; Parse { val1 val2 ... } or { (v1 v2) (v3 v4) ... }
               (when (and toks (string= (car toks) "{"))
                 (pop toks)
                 (loop until (or (null toks) (string= (car toks) "}")) do
                   (if (and (stringp (car toks)) (string= (car toks) "("))
                       (progn
                         (pop toks)
                         (let ((row nil))
                           (loop until (or (null toks) (string= (car toks) ")")) do
                             (let ((tok (pop toks)))
                               (push (if (and (stringp tok) (string-equal tok "UNDEF"))
                                         nil
                                         (sparql-resolve-term tok prefixes))
                                     row)))
                           (when toks (pop toks))
                           (push (nreverse row) val-data)))
                       (let ((tok (pop toks)))
                         (push (list (if (and (stringp tok) (string-equal tok "UNDEF"))
                                        nil
                                        (sparql-resolve-term tok prefixes)))
                               val-data))))
                 (when toks (pop toks)))
               (push (list 'values val-vars (nreverse val-data)) clauses)))
            (t (return))))
        ;; Build DSL expression
        (let* ((ne-and-graph (remove-if-not (lambda (p) (and (consp p) (member (car p) '(not-exists exists graph minus)))) patterns))
               (clean-patterns (remove-if (lambda (p) (and (consp p) (member (car p) '(not-exists exists graph minus)))) patterns))
               (expr (list (if distinct-p 'select-distinct 'select)
                          vars
                          (cons 'where clean-patterns))))
          ;; Extract NOT-EXISTS and GRAPH from patterns
          (dolist (p ne-and-graph)
            (setf expr (append expr (list p))))
          (dolist (opt optionals)
            (setf expr (append expr (list opt))))
          (dolist (u unions)
            (setf expr (append expr (list u))))
          (when filters
            (setf expr (append expr (list (cons 'filter filters)))))
          (dolist (b binds)
            (setf expr (append expr (list b))))
          (dolist (p projections)
            (let* ((toks (cdr p))
                   ;; Parse expression tokens into evaluable form
                   (parsed (parse-projection-expr toks prefixes)))
              (setf expr (append expr (list (list 'project (car p) parsed))))))
          (dolist (c clauses)
            (setf expr (append expr (list c))))
          expr)))))

(defun parse-projection-expr (toks prefixes)
  "Parse expression tokens from SELECT (expr AS ?var) into evaluable form."
  (multiple-value-bind (expr rest) (parse-or-expr toks prefixes)
    (declare (ignore rest))
    expr))

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
  "Parse comparison: expr op expr, or expr IN/NOT IN (list)"
  (multiple-value-bind (left rest) (parse-additive-expr toks prefixes)
    (cond
      ;; expr IN (val, ...)
      ((and rest (stringp (car rest)) (string-equal (car rest) "IN"))
       (pop rest)
       (let ((vals (parse-in-list rest prefixes)))
         (setf left (list 'in left (car vals)))
         (setf rest (cdr vals))))
      ;; expr NOT IN (val, ...)
      ((and rest (stringp (car rest)) (string-equal (car rest) "NOT")
            (cdr rest) (stringp (cadr rest)) (string-equal (cadr rest) "IN"))
       (pop rest) (pop rest)
       (let ((vals (parse-in-list rest prefixes)))
         (setf left (list 'not (list 'in left (car vals))))
         (setf rest (cdr vals))))
      ;; Regular comparison
      ((and rest (symbolp (car rest))
            (member (car rest) '(= != < > <= >=) :test #'eq))
       (let ((op (pop rest)))
         (multiple-value-bind (right rest2) (parse-additive-expr rest prefixes)
           (setf left (list op left right))
           (setf rest rest2)))))
    (values left rest)))

(defun parse-additive-expr (toks prefixes)
  "Parse additive: expr (+|-) expr"
  (multiple-value-bind (left rest) (parse-multiplicative-expr toks prefixes)
    (loop while (and rest
                     (or (and (symbolp (car rest)) (member (car rest) '(+ -) :test #'eq))
                         (and (stringp (car rest)) (member (car rest) '("+" "-") :test #'string=)))) do
      (let ((op (if (stringp (car rest)) (intern (pop rest)) (pop rest))))
        (multiple-value-bind (right rest2) (parse-multiplicative-expr rest prefixes)
          (setf left (list op left right))
          (setf rest rest2))))
    (values left rest)))

(defun parse-multiplicative-expr (toks prefixes)
  "Parse multiplicative: expr (*|/) expr"
  (multiple-value-bind (left rest) (parse-unary-expr toks prefixes)
    (loop while (and rest (stringp (car rest))
                     (member (car rest) '("*" "/") :test #'string=)) do
      (let ((op (intern (pop rest))))
        (multiple-value-bind (right rest2) (parse-unary-expr rest prefixes)
          (setf left (list op left right))
          (setf rest rest2))))
    (values left rest)))

(defun parse-in-list (toks prefixes)
  "Parse (val1, val2, ...). Returns (list-of-values . remaining-toks)."
  (when (and toks (stringp (car toks)) (string= (car toks) "("))
    (pop toks))
  (let ((vals nil))
    (loop until (or (null toks) (and (stringp (car toks)) (string= (car toks) ")"))) do
      (multiple-value-bind (v rest) (parse-or-expr toks prefixes)
        (push v vals)
        (setf toks rest))
      (when (and toks (stringp (car toks)) (string= (car toks) ","))
        (pop toks)))
    (when (and toks (stringp (car toks)) (string= (car toks) ")"))
      (pop toks))
    (cons (nreverse vals) toks)))

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
    ;; Asterisk (for COUNT(*))
    ((and (stringp (car toks)) (string= (car toks) "*"))
     (pop toks) (values '* toks))
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
       ;; Handle DISTINCT in aggregates: COUNT(DISTINCT ?x)
       (let ((distinct-agg nil))
         (when (and toks (stringp (car toks)) (string-equal (car toks) "DISTINCT"))
           (pop toks)
           (setf distinct-agg t))
         (let ((args nil))
           (loop until (or (null toks)
                           (and (stringp (car toks)) (string= (car toks) ")"))
                           (and (stringp (car toks)) (string= (car toks) ";"))) do
             (multiple-value-bind (arg rest) (parse-or-expr toks prefixes)
               (push arg args)
               (setf toks rest))
             (when (and toks (stringp (car toks)) (string= (car toks) ","))
               (pop toks)))
           ;; Handle SEPARATOR in GROUP_CONCAT
           (let ((separator nil))
             (when (and toks (stringp (car toks)) (string= (car toks) ";"))
               (pop toks)
               (when (and toks (stringp (car toks)) (string-equal (car toks) "SEPARATOR"))
                 (pop toks)
                 (when toks
                   (let ((eq-tok (car toks)))
                     (when (or (and (stringp eq-tok) (string= eq-tok "="))
                               (and (symbolp eq-tok) (string= (symbol-name eq-tok) "=")))
                       (pop toks)
                       (setf separator (sparql-resolve-term (pop toks) prefixes)))))))
             (when (and toks (stringp (car toks)) (string= (car toks) ")"))
               (pop toks))
             (let ((result (cons (intern fname) (nreverse args))))
               (when distinct-agg
                 (setf result (list (intern (concatenate 'string fname "-DISTINCT"))
                                    (second result))))
               (when separator
                 (setf result (append result (list separator))))
               (values result toks)))))))
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
             ;; FILTER EXISTS { ... }
             (if (and toks (stringp (car toks)) (string-equal (car toks) "EXISTS"))
                 (progn
                   (pop toks)
                   (when (and toks (stringp (car toks)) (string= (car toks) "{"))
                     (pop toks))
                   (multiple-value-bind (e-pats e-filts e-rest)
                       (sparql-parse-body toks prefixes)
                     (declare (ignore e-filts))
                     (setf toks e-rest)
                     (when (and toks (stringp (car toks)) (string= (car toks) "}"))
                       (pop toks))
                     (push (cons 'exists e-pats) patterns)))
                 ;; Regular FILTER expression
                 (multiple-value-bind (expr rest) (parse-sparql-filter-expr toks prefixes)
                   (setf toks rest)
                   (when expr (push expr filters)))))
         (when (and toks (stringp (car toks)) (string= (car toks) "."))
           (pop toks)))
        ;; BIND (expr AS ?var)
        ((string-equal (car toks) "BIND")
         (pop toks)
         (when (and toks (stringp (car toks)) (string= (car toks) "("))
           (pop toks))
         ;; Parse expression using full expression parser
         (multiple-value-bind (expr rest) (parse-or-expr toks prefixes)
           (setf toks rest)
           (when (and toks (stringp (car toks)) (string-equal (car toks) "AS"))
             (pop toks))
           (let ((var (pop toks)))
             (push (list 'bind var expr) binds)
             (push (list 'inline-bind var expr) patterns))
           (when (and toks (stringp (car toks)) (string= (car toks) ")"))
             (pop toks))
           (when (and toks (stringp (car toks)) (string= (car toks) "."))
             (pop toks))))
        ;; VALUES inside WHERE clause
        ((string-equal (car toks) "VALUES")
         (pop toks)
         (let ((val-vars nil) (val-data nil))
           (if (and toks (stringp (car toks)) (string= (car toks) "("))
               (progn
                 (pop toks)
                 (loop until (or (null toks) (string= (car toks) ")")) do
                   (push (pop toks) val-vars))
                 (when toks (pop toks))
                 (setf val-vars (nreverse val-vars)))
               (push (pop toks) val-vars))
           (when (and toks (string= (car toks) "{"))
             (pop toks)
             (loop until (or (null toks) (string= (car toks) "}")) do
               (if (and (stringp (car toks)) (string= (car toks) "("))
                   (progn
                     (pop toks)
                     (let ((row nil))
                       (loop until (or (null toks) (string= (car toks) ")")) do
                         (let ((tok (pop toks)))
                           (push (if (and (stringp tok) (string-equal tok "UNDEF"))
                                     nil (sparql-resolve-term tok prefixes))
                                 row)))
                       (when toks (pop toks))
                       (push (nreverse row) val-data)))
                   (let ((tok (pop toks)))
                     (push (list (if (and (stringp tok) (string-equal tok "UNDEF"))
                                     nil (sparql-resolve-term tok prefixes)))
                           val-data))))
             (when toks (pop toks)))
           (push (list 'values val-vars (nreverse val-data)) patterns)))
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
        ;; GRAPH <uri> { ... }
        ((string-equal (car toks) "GRAPH")
         (pop toks)
         (let ((graph-uri (sparql-resolve-term (pop toks) prefixes)))
           (when (and toks (stringp (car toks)) (string= (car toks) "{"))
             (pop toks))
           (multiple-value-bind (g-pats g-filts g-rest)
               (sparql-parse-body toks prefixes)
             (declare (ignore g-filts))
             (setf toks g-rest)
             (when (and toks (stringp (car toks)) (string= (car toks) "}"))
               (pop toks))
             (push (list 'graph graph-uri g-pats) patterns))))
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
        ;; MINUS { ... }
        ((string-equal (car toks) "MINUS")
         (pop toks)
         (when (and toks (string= (car toks) "{"))
           (pop toks))
         (multiple-value-bind (m-pats m-filts m-rest m-opts)
             (sparql-parse-body toks prefixes)
           (setf toks m-rest)
           (when (and toks (string= (car toks) "}"))
             (pop toks))
           (let ((clause (cons 'minus m-pats)))
             (when m-filts (nconc clause (list (cons 'filter m-filts))))
             (when m-opts (nconc clause m-opts))
             (push clause patterns))))
        ;; UNION or nested block: { ... } UNION { ... } or { SELECT subquery }
        ((string= (car toks) "{")
         (pop toks)
         ;; Check for subquery: { SELECT ... WHERE { ... } }
         (if (and toks (stringp (car toks)) (string-equal (car toks) "SELECT"))
             (progn
               ;; Collect subquery tokens until matching }
               (let ((sub-toks nil) (depth 1))
                 (loop while (and toks (> depth 0)) do
                   (let ((tok (pop toks)))
                     (when (stringp tok)
                       (cond ((string= tok "{") (incf depth))
                             ((string= tok "}") (decf depth))))
                     (when (> depth 0) (push tok sub-toks))))
                 (let ((st (nreverse sub-toks)))
                   ;; Skip leading SELECT keyword
                   (when (and st (stringp (car st)) (string-equal (car st) "SELECT"))
                     (pop st))
                   (push (list 'subquery (sparql-parse-select st prefixes nil)) patterns))))
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
         (let ((s-tok (pop toks)))
           ;; Handle blank node [ pred obj ; ... ] as subject or object
           (when (and (stringp s-tok) (string= s-tok "["))
             (let ((bnode (intern (format nil "?_ANON~A" (incf *sparql-anon-counter*)))))
               ;; Parse predicate-object pairs inside [ ]
               (loop while (and toks (not (string= (car toks) "]"))) do
                 (let* ((bp-result (parse-sparql-path toks prefixes))
                        (bp (car bp-result))
                        (bo-toks (cdr bp-result))
                        (bo-tok (pop bo-toks))
                        (bo (if (and (stringp bo-tok) (string= bo-tok "["))
                                ;; Nested blank node
                                (let ((inner (intern (format nil "?_ANON~A" (incf *sparql-anon-counter*)))))
                                  (loop while (and bo-toks (not (string= (car bo-toks) "]"))) do
                                    (pop bo-toks))
                                  (when bo-toks (pop bo-toks))
                                  inner)
                                (let ((v (sparql-resolve-term bo-tok prefixes)))
                                  (if (numberp v) (intern-literal v (if (integerp v) +xsd-integer+ +xsd-decimal+)) v)))))
                   (setf toks bo-toks)
                   (push (list bnode bp bo) patterns)
                   (when (and toks (stringp (car toks)) (string= (car toks) ";"))
                     (pop toks))))
               (when (and toks (string= (car toks) "]"))
                 (pop toks))
               (setf s-tok bnode)))
           (let* ((s (sparql-resolve-term s-tok prefixes))
                ;; Parse property path expression
                (path-result (parse-sparql-path toks prefixes))
                (p (car path-result))
                (o-toks (cdr path-result))
                (o (let ((o-tok (pop o-toks)))
                     (if (and (stringp o-tok) (string= o-tok "["))
                         ;; Blank node as object
                         (let ((bnode (intern (format nil "?_ANON~A" (incf *sparql-anon-counter*)))))
                           (loop while (and o-toks (not (string= (car o-toks) "]"))) do
                             (let* ((bp-r (parse-sparql-path o-toks prefixes))
                                    (bp (car bp-r))
                                    (bo-toks (cdr bp-r))
                                    (bo (let ((v (sparql-resolve-term (pop bo-toks) prefixes)))
                                          (if (numberp v) (intern-literal v (if (integerp v) +xsd-integer+ +xsd-decimal+)) v))))
                               (setf o-toks bo-toks)
                               (push (list bnode bp bo) patterns)
                               (when (and o-toks (stringp (car o-toks)) (string= (car o-toks) ";"))
                                 (pop o-toks))))
                           (when (and o-toks (string= (car o-toks) "]"))
                             (pop o-toks))
                           bnode)
                         (let ((v (sparql-resolve-term o-tok prefixes)))
                           (if (numberp v) (intern-literal v (if (integerp v) +xsd-integer+ +xsd-decimal+)) v))))))
           (setf toks o-toks)
           (push (list s p o) patterns)
           ;; Handle ; (same subject, new predicate-object pairs)
           (loop while (and toks (stringp (car toks)) (string= (car toks) ";")
                            (cdr toks) (not (string= (cadr toks) "}"))) do
             (pop toks)
             (let* ((pr (parse-sparql-path toks prefixes))
                    (p2 (car pr))
                    (o2-toks (cdr pr))
                    (o2 (let ((o2-tok (pop o2-toks)))
                          (if (and (stringp o2-tok) (string= o2-tok "["))
                              (let ((bnode (intern (format nil "?_ANON~A" (incf *sparql-anon-counter*)))))
                                (loop while (and o2-toks (not (string= (car o2-toks) "]"))) do
                                  (let* ((bp-r (parse-sparql-path o2-toks prefixes))
                                         (bp (car bp-r))
                                         (bo-toks (cdr bp-r))
                                         (bo (let ((v (sparql-resolve-term (pop bo-toks) prefixes)))
                                               (if (numberp v) (intern-literal v (if (integerp v) +xsd-integer+ +xsd-decimal+)) v))))
                                    (setf o2-toks bo-toks)
                                    (push (list bnode bp bo) patterns)
                                    (when (and o2-toks (stringp (car o2-toks)) (string= (car o2-toks) ";"))
                                      (pop o2-toks))))
                                (when (and o2-toks (string= (car o2-toks) "]"))
                                  (pop o2-toks))
                                bnode)
                              (let ((v (sparql-resolve-term o2-tok prefixes)))
                                (if (numberp v) (intern-literal v (if (integerp v) +xsd-integer+ +xsd-decimal+)) v))))))
               (setf toks o2-toks)
               (push (list s p2 o2) patterns)
               ;; Handle , (same subject and predicate, new object)
               (loop while (and toks (stringp (car toks)) (string= (car toks) ",")) do
                 (pop toks)
                 (let ((o3 (let ((v (sparql-resolve-term (pop toks) prefixes)))
                             (if (numberp v) (intern-literal v (if (integerp v) +xsd-integer+ +xsd-decimal+)) v))))
                   (push (list s p2 o3) patterns)))))
           ;; Handle , after first triple (same subject and predicate, new object)
           (loop while (and toks (stringp (car toks)) (string= (car toks) ",")) do
             (pop toks)
             (let ((o2 (let ((v (sparql-resolve-term (pop toks) prefixes)))
                         (if (numberp v) (intern-literal v (if (integerp v) +xsd-integer+ +xsd-decimal+)) v))))
               (push (list s p o2) patterns)))))
         (when (and toks (stringp (car toks)) (string= (car toks) "."))
           (pop toks)))))
    (values (nreverse patterns) (nreverse filters) toks (nreverse optionals) (nreverse unions) (nreverse binds))))

(defun parse-sparql-path (toks prefixes)
  "Parse a SPARQL property path expression. Returns (path . remaining-toks)."
  ;; path-alt := path-seq ( '|' path-seq )*
  (let ((left (parse-sparql-path-seq toks prefixes)))
    (loop while (and (cdr left) (stringp (cadr left)) (string= (cadr left) "|")) do
      (pop (cdr left))  ; consume |
      (let ((right (parse-sparql-path-seq (cdr left) prefixes)))
        (setf left (cons (list 'alt (car left) (car right)) (cdr right)))))
    left))

(defun parse-sparql-path-seq (toks prefixes)
  "Parse path-seq := path-elt ( '/' path-elt )*. Returns (path . remaining-toks)."
  (let ((left (parse-sparql-path-elt toks prefixes)))
    (loop while (and (cdr left) (stringp (cadr left)) (string= (cadr left) "/")) do
      (pop (cdr left))  ; consume /
      (let ((right (parse-sparql-path-elt (cdr left) prefixes)))
        (setf left (cons (list 'seq (car left) (car right)) (cdr right)))))
    left))

(defun parse-sparql-path-elt (toks prefixes)
  "Parse path-elt := '^'? primary ('*'|'+'|'?')?. Returns (path . remaining-toks)."
  (let ((inverse-p (when (and toks (stringp (car toks)) (string= (car toks) "^"))
                     (pop toks) t))
        (negated-p (when (and toks (symbolp (car toks)) (string= (symbol-name (car toks)) "!"))
                     (pop toks) t)))
    ;; Primary: URI, 'a', or '(' path ')'
    (let* ((primary
             (cond
               ((and toks (stringp (car toks)) (string= (car toks) "("))
                (pop toks)
                (let ((inner (parse-sparql-path toks prefixes)))
                  (setf toks (cdr inner))
                  (when (and toks (stringp (car toks)) (string= (car toks) ")"))
                    (pop toks))
                  (car inner)))
               (t (let ((raw (pop toks)))
                    (let ((r (sparql-resolve-term raw prefixes)))
                      (if (equal r "a") "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" r))))))
           ;; Modifier: * + ?
           (modifier (when (and toks (stringp (car toks))
                                (member (car toks) '("+" "*" "?") :test #'string=))
                       (pop toks)))
           (path primary))
      (when negated-p (setf path (list 'neg path)))
      (when inverse-p (setf path (list 'inv path)))
      (when modifier
        (setf path (cond ((string= modifier "+") (list '+ path))
                         ((string= modifier "*") (list '* path))
                         ((string= modifier "?") (list 'zeroOrOne path)))))
      (cons path toks))))

(defun sparql-resolve-term (term prefixes)
  "Resolve a SPARQL term: expand prefixed names, keep variables as symbols."
  (cond
    ((null term) nil)
    ((symbolp term) term)  ; ?variable
    ((numberp term) term)
    ((rdf-literal-p term) term)
    ((stringp term)
     (cond
       ;; Quoted string literal
       ((and (> (length term) 1) (char= #\" (char term 0)))
        (resolve-sparql-token term prefixes))
       ;; Prefixed name (contains : but not ://)
       ((and (position #\: term) (not (search "://" term)))
        (let* ((colon (position #\: term))
               (prefix (concatenate 'string (subseq term 0 (1+ colon))))
               (local (subseq term (1+ colon)))
               (base (gethash prefix prefixes)))
          (if base (concatenate 'string base local) term)))
       (t term)))
    (t term)))


(defun sparql-parse-construct (toks prefixes)
  "Parse CONSTRUCT { template } WHERE { patterns } or CONSTRUCT WHERE { patterns }."
  ;; Check for CONSTRUCT WHERE shorthand (with optional FROM)
  (if (and toks (stringp (car toks))
          (or (string-equal (car toks) "WHERE")
              (string-equal (car toks) "FROM")))
      (progn
        ;; Skip FROM clauses
        (loop while (and toks (stringp (car toks)) (string-equal (car toks) "FROM")) do
          (pop toks)
          (when (and toks (stringp (car toks)) (string-equal (car toks) "NAMED"))
            (pop toks))
          (pop toks))
        (when (and toks (stringp (car toks)) (string-equal (car toks) "WHERE"))
          (pop toks))
        (when (and toks (string= (car toks) "{"))
          (pop toks))
        (multiple-value-bind (patterns filters toks-rest)
            (sparql-parse-body toks prefixes)
          (declare (ignore filters toks-rest))
          ;; Template = WHERE patterns
          (list 'construct patterns (cons 'where patterns))))
      ;; Normal CONSTRUCT { template } WHERE { patterns }
      (progn
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
          (when (and toks (string-equal (car toks) "WHERE"))
            (pop toks))
          (when (and toks (string= (car toks) "{"))
            (pop toks))
          (multiple-value-bind (patterns filters toks-rest)
              (sparql-parse-body toks prefixes)
            (declare (ignore filters toks-rest))
            (list 'construct (nreverse template) (cons 'where patterns)))))))

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
     (let* ((end (position #\" tok :start 1))
            (str (subseq tok 1 end))
            (rest (when end (subseq tok (1+ end)))))
       (cond
         ((and rest (>= (length rest) 2) (string= "^^" (subseq rest 0 2)))
          (let* ((type-tok (subseq rest 2))
                 (type-uri (if (and (> (length type-tok) 0) (char= #\< (char type-tok 0)))
                               (subseq type-tok 1 (1- (length type-tok)))
                               (let* ((colon (position #\: type-tok))
                                      (prefix (when colon (subseq type-tok 0 (1+ colon))))
                                      (local (when colon (subseq type-tok (1+ colon))))
                                      (base (when prefix (gethash prefix prefixes))))
                                 (if base (concatenate 'string base local) type-tok)))))
            (convert-typed-literal str type-uri)))
         ((and rest (> (length rest) 0) (char= #\@ (char rest 0)))
          (intern-literal str +rdf-langstring+ (string-downcase (subseq rest 1))))
         (t (intern-literal str +xsd-string+)))))
    ((position #\: tok)
     (let* ((colon (position #\: tok))
            (prefix (subseq tok 0 (1+ colon)))
            (local (subseq tok (1+ colon)))
            (base (gethash prefix prefixes)))
       (if base (concatenate 'string base local) tok)))
    (t tok)))
