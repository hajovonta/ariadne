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
              ((member ch '(#\{ #\} #\( #\) #\.))
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
              ((char= ch #\<)
               (let ((end (position #\> str :start (1+ pos))))
                 (when end
                   (push (subseq str (1+ pos) end) tokens)
                   (setf pos (1+ end)))))
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
              ;; Comparison operators
              ((member ch '(#\> #\< #\= #\!))
               (let ((start pos))
                 (incf pos)
                 (when (and (< pos len) (char= #\= (char str pos)))
                   (incf pos))
                 (push (intern (subseq str start pos)) tokens)))
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
        (t (error "Unknown SPARQL query form: ~A" form))))))

(defun sparql-parse-select (toks prefixes distinct-p)
  "Parse SELECT query."
  ;; Check for DISTINCT
  (when (and toks (string-equal (car toks) "DISTINCT"))
    (pop toks)
    (setf distinct-p t))
  ;; Parse variable list
  (let ((vars nil))
    (loop while (and toks (symbolp (car toks)) (char= #\? (char (symbol-name (car toks)) 0))) do
      (push (pop toks) vars))
    (setf vars (nreverse vars))
    ;; Expect WHERE
    (when (and toks (string-equal (car toks) "WHERE"))
      (pop toks))
    ;; Expect {
    (when (and toks (string= (car toks) "{"))
      (pop toks))
    ;; Parse patterns and filters
    (multiple-value-bind (patterns filters toks-rest)
        (sparql-parse-body toks prefixes)
      (setf toks toks-rest)
      ;; Expect }
      (when (and toks (string= (car toks) "}"))
        (pop toks))
      ;; Parse trailing clauses (LIMIT, ORDER BY, etc.)
      (let ((clauses nil))
        (loop while toks do
          (cond
            ((string-equal (car toks) "LIMIT")
             (pop toks)
             (push (list 'limit (pop toks)) clauses))
            ((string-equal (car toks) "ORDER")
             (pop toks)
             (when (and toks (string-equal (car toks) "BY"))
               (pop toks))
             (push (list 'order-by (pop toks)) clauses))
            (t (return))))
        ;; Build DSL expression
        (let ((expr (list (if distinct-p 'select-distinct 'select)
                          vars
                          (cons 'where patterns))))
          (when filters
            (setf expr (append expr (list (cons 'filter filters)))))
          (dolist (c clauses)
            (setf expr (append expr (list c))))
          expr)))))

(defun sparql-parse-ask (toks prefixes)
  "Parse ASK query."
  ;; Expect {
  (when (and toks (string= (car toks) "{"))
    (pop toks))
  (multiple-value-bind (patterns filters toks-rest)
      (sparql-parse-body toks prefixes)
    (declare (ignore filters toks-rest))
    (list 'ask (cons 'where patterns))))

(defun sparql-parse-body (toks prefixes)
  "Parse the body of a WHERE clause. Returns (values patterns filters remaining-toks)."
  (let ((patterns nil)
        (filters nil))
    (loop while (and toks (not (string= (car toks) "}"))) do
      (cond
        ;; FILTER
        ((string-equal (car toks) "FILTER")
         (pop toks)
         ;; Expect (
         (when (and toks (string= (car toks) "("))
           (pop toks))
         ;; Parse filter expression: ?var op value
         (let ((left (sparql-resolve-term (pop toks) prefixes))
               (op (intern (string-upcase (princ-to-string (pop toks)))))
               (right (sparql-resolve-term (pop toks) prefixes)))
           (push (list op left right) filters))
         ;; Expect )
         (when (and toks (string= (car toks) ")"))
           (pop toks)))
        ;; Triple pattern: s p o .
        (t
         (let ((s (sparql-resolve-term (pop toks) prefixes))
               (p (sparql-resolve-term (pop toks) prefixes))
               (o (sparql-resolve-term (pop toks) prefixes)))
           (push (list s p o) patterns))
         ;; Skip optional .
         (when (and toks (string= (car toks) "."))
           (pop toks)))))
    (values (nreverse patterns) (nreverse filters) toks)))

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
