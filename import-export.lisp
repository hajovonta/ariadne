;;;; import-export.lisp
;;;; N-Triples, Turtle, N-Quads import/export

(in-package #:ariadne)

;;; ==========================================================================
;;; N-Triples Import
;;; ==========================================================================

(defun import-ntriples (g data)
  "Import N-Triples format string into graph G."
  (with-input-from-string (s data)
    (loop for line = (read-line s nil nil)
          while line do
          (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
            (when (and (> (length trimmed) 0)
                       (char/= #\# (char trimmed 0)))
              (multiple-value-bind (subj pred obj)
                  (parse-ntriple-line trimmed)
                (when (and subj pred obj)
                  (add-triple g subj pred obj))))))))

(defun import-ntriples-file (g path)
  "Import N-Triples from a file."
  (import-ntriples g (uiop:read-file-string path)))

(defun parse-ntriple-line (line)
  "Parse a single N-Triples line into subject, predicate, object."
  ;; Remove trailing " ."
  (let ((line (string-right-trim '(#\Space #\Tab) line)))
    (when (and (> (length line) 1)
               (char= #\. (char line (1- (length line)))))
      (setf line (string-right-trim '(#\Space #\Tab)
                                     (subseq line 0 (1- (length line))))))
    (let ((tokens (tokenize-ntriple line)))
      (when (>= (length tokens) 3)
        (values (first tokens) (second tokens) (third tokens))))))

(defun tokenize-ntriple (line)
  "Tokenize an N-Triples line into components."
  (let ((tokens nil)
        (pos 0)
        (len (length line)))
    (flet ((skip-ws ()
             (loop while (and (< pos len)
                              (member (char line pos) '(#\Space #\Tab)))
                   do (incf pos))))
      (loop while (< pos len) do
        (skip-ws)
        (when (< pos len)
          (cond
            ;; URI: <...>
            ((char= #\< (char line pos))
             (let ((end (position #\> line :start (1+ pos))))
               (when end
                 (push (subseq line (1+ pos) end) tokens)
                 (setf pos (1+ end)))))
            ;; Literal: "..."
            ((char= #\" (char line pos))
             (let ((str (parse-ntriple-string line pos)))
               (push (car str) tokens)
               (setf pos (cdr str))))
            ;; Blank node: _:...
            ((and (< (1+ pos) len)
                  (char= #\_ (char line pos))
                  (char= #\: (char line (1+ pos))))
             (let ((end (or (position #\Space line :start pos)
                            len)))
               (push (subseq line pos end) tokens)
               (setf pos end)))
            (t (incf pos))))))
    (nreverse tokens)))

(defun parse-ntriple-string (line start)
  "Parse a quoted string starting at START. Returns (value . end-pos)."
  (let ((pos (1+ start))
        (len (length line))
        (chars nil))
    ;; Read until closing quote
    (loop while (and (< pos len) (char/= #\" (char line pos))) do
      (if (char= #\\ (char line pos))
          (progn (incf pos)
                 (when (< pos len) (push (char line pos) chars) (incf pos)))
          (progn (push (char line pos) chars) (incf pos))))
    (when (< pos len) (incf pos)) ; skip closing quote
    ;; Check for type annotation ^^<...> or language tag @...
    (let ((value (coerce (nreverse chars) 'string)))
      (cond
        ;; Typed literal: "30"^^<xsd:integer>
        ((and (< (1+ pos) len)
              (char= #\^ (char line pos))
              (char= #\^ (char line (1+ pos))))
         (setf pos (+ pos 2))
         (when (and (< pos len) (char= #\< (char line pos)))
           (let ((end (position #\> line :start pos)))
             (when end
               (let ((type-uri (subseq line (1+ pos) end)))
                 (setf pos (1+ end))
                 ;; Convert typed literals
                 (setf value (convert-typed-literal value type-uri)))))))
        ;; Language tag: "Alice"@en
        ((and (< pos len) (char= #\@ (char line pos)))
         (let ((end (or (position #\Space line :start pos) len)))
           (setf pos end))))
      (cons value pos))))

(defun convert-typed-literal (value type-uri)
  "Convert a string value to the appropriate CL type based on XSD type URI."
  (cond
    ((search "integer" type-uri) (parse-integer value))
    ((search "decimal" type-uri) (read-from-string value))
    ((search "float" type-uri) (read-from-string value))
    ((search "double" type-uri) (read-from-string value))
    ((search "boolean" type-uri) (string= value "true"))
    (t value)))

;;; ==========================================================================
;;; N-Triples Export
;;; ==========================================================================

(defun export-ntriples (g)
  "Export graph as N-Triples format string."
  (with-output-to-string (s)
    (dolist (tr (get-triples g))
      (format s "~A ~A ~A .~%"
              (format-nt-term (triple-subject tr))
              (format-nt-term (triple-predicate tr))
              (format-nt-term (triple-object tr))))))

(defun format-nt-term (term)
  "Format a term for N-Triples output."
  (cond
    ((stringp term)
     (if (and (> (length term) 0)
              (or (search "://" term)
                  (and (> (length term) 2)
                       (char= #\_ (char term 0))
                       (char= #\: (char term 1)))))
         (format nil "<~A>" term)
         (format nil "\"~A\"" term)))
    ((numberp term)
     (format nil "\"~A\"^^<http://www.w3.org/2001/XMLSchema#~A>"
             term (if (integerp term) "integer" "decimal")))
    (t (format nil "<~A>" term))))

;;; ==========================================================================
;;; Turtle Import (simplified)
;;; ==========================================================================

(defun import-turtle (g data)
  "Import Turtle format string into graph G.
Tokenizes the entire input then processes token stream."
  (let ((prefixes (make-hash-table :test 'equal))
        (tokens (turtle-tokenize data)))
    (turtle-parse-tokens g tokens prefixes)))

(defun turtle-tokenize (data)
  "Tokenize Turtle input into a flat list of tokens.
Handles quoted strings, URIs, and punctuation (; , .)."
  (let ((tokens nil)
        (pos 0)
        (len (length data)))
    (flet ((skip-ws ()
             (loop while (and (< pos len)
                              (member (char data pos) '(#\Space #\Tab #\Newline #\Return)))
                   do (incf pos)))
           (skip-comment ()
             (loop while (and (< pos len) (char/= #\Newline (char data pos)))
                   do (incf pos))))
      (loop while (< pos len) do
        (skip-ws)
        (when (< pos len)
          (let ((ch (char data pos)))
            (cond
              ;; Comment
              ((char= ch #\#) (skip-comment))
              ;; Punctuation
              ((member ch '(#\. #\; #\,))
               (push (string ch) tokens)
               (incf pos))
              ;; URI <...>
              ((char= ch #\<)
               (let ((end (position #\> data :start (1+ pos))))
                 (if end
                     (progn (push (subseq data pos (1+ end)) tokens)
                            (setf pos (1+ end)))
                     (incf pos))))
              ;; String "..."
              ((char= ch #\")
               (let ((start pos))
                 (incf pos)
                 (loop while (and (< pos len) (char/= #\" (char data pos)))
                       do (when (char= #\\ (char data pos)) (incf pos))
                          (incf pos))
                 (when (< pos len) (incf pos)) ; closing quote
                 ;; Check for ^^type or @lang
                 (when (and (< (1+ pos) len)
                            (char= #\^ (char data pos))
                            (char= #\^ (char data (1+ pos))))
                   (incf pos 2)
                   (when (and (< pos len) (char= #\< (char data pos)))
                     (let ((end (position #\> data :start pos)))
                       (when end (setf pos (1+ end))))))
                 (when (and (< pos len) (char= #\@ (char data pos)))
                   (loop while (and (< pos len)
                                    (not (member (char data pos)
                                                 '(#\Space #\Tab #\Newline #\Return
                                                   #\. #\; #\,))))
                         do (incf pos)))
                 (push (subseq data start pos) tokens)))
              ;; @prefix / @base
              ((char= ch #\@)
               (let ((start pos))
                 (loop while (and (< pos len)
                                  (not (member (char data pos)
                                               '(#\Space #\Tab #\Newline #\Return))))
                       do (incf pos))
                 (push (subseq data start pos) tokens)))
              ;; Blank node _:...
              ((and (char= ch #\_) (< (1+ pos) len) (char= #\: (char data (1+ pos))))
               (let ((start pos))
                 (loop while (and (< pos len)
                                  (not (member (char data pos)
                                               '(#\Space #\Tab #\Newline #\Return
                                                 #\. #\; #\,))))
                       do (incf pos))
                 (push (subseq data start pos) tokens)))
              ;; Other token (prefixed name, number, etc.)
              (t
               (let ((start pos))
                 (loop while (and (< pos len)
                                  (not (member (char data pos)
                                               '(#\Space #\Tab #\Newline #\Return
                                                 #\. #\; #\,))))
                       do (incf pos))
                 (when (> pos start)
                   (push (subseq data start pos) tokens)))))))))
    (nreverse tokens)))

(defun turtle-parse-tokens (g tokens prefixes)
  "Parse a token stream into triples."
  (let ((toks tokens)
        (subject nil)
        (predicate nil))
    (loop while toks do
      (let ((tok (car toks)))
        (cond
          ;; @prefix
          ((string-equal tok "@prefix")
           (pop toks)
           (let ((prefix-name (pop toks))
                 (uri (pop toks)))
             ;; prefix-name is like "ex:" and uri is like "<http://...>"
             (setf (gethash prefix-name prefixes)
                   (subseq uri 1 (1- (length uri))))
             ;; skip the "."
             (when (and toks (string= "." (car toks)))
               (pop toks))))
          ;; "."  — end of statement
          ((string= tok ".")
           (pop toks)
           (setf subject nil predicate nil))
          ;; ";" — same subject, new predicate
          ((string= tok ";")
           (pop toks)
           (setf predicate nil))
          ;; "," — same subject and predicate, new object
          ((string= tok ",")
           (pop toks)
           (when (and subject predicate toks)
             (let ((obj (turtle-resolve (pop toks) prefixes)))
               (add-triple g subject predicate obj))))
          ;; Regular token
          (t
           (cond
             ;; Need subject
             ((null subject)
              (setf subject (turtle-resolve (pop toks) prefixes)))
             ;; Need predicate
             ((null predicate)
              (setf predicate (turtle-resolve (pop toks) prefixes)))
             ;; Have both — this is the object
             (t
              (let ((obj (turtle-resolve (pop toks) prefixes)))
                (add-triple g subject predicate obj))))))))))

(defun turtle-resolve (token prefixes)
  "Resolve a Turtle token to a value."
  (flet ((expand-prefix (term)
           (let ((colon-pos (position #\: term)))
             (if colon-pos
                 (let* ((prefix (concatenate 'string (subseq term 0 (1+ colon-pos))))
                        (local (subseq term (1+ colon-pos)))
                        (base (gethash prefix prefixes)))
                   (if base (concatenate 'string base local) term))
                 term))))
    (cond
    ;; URI: <http://...>
    ((and (> (length token) 1)
          (char= #\< (char token 0)))
     (subseq token 1 (1- (length token))))
    ;; Quoted string (possibly with type/lang)
    ((and (> (length token) 0)
          (char= #\" (char token 0)))
     (let ((end-quote (position #\" token :start 1)))
       (if end-quote
           (let ((str (subseq token 1 end-quote))
                 (rest (subseq token (1+ end-quote))))
             (cond
               ((and (>= (length rest) 2)
                     (string= "^^" (subseq rest 0 2)))
                (let ((type-uri (string-trim '(#\< #\>) (subseq rest 2))))
                  (convert-typed-literal str type-uri)))
               (t str)))
           token)))
    ;; Number
    ((and (> (length token) 0)
          (or (digit-char-p (char token 0))
              (and (> (length token) 1)
                   (char= #\- (char token 0))
                   (digit-char-p (char token 1)))))
     (let ((val (read-from-string token)))
       (if (numberp val) val token)))
    ;; Prefixed name
    ((position #\: token)
     (expand-prefix token))
    ;; 'a' shorthand for rdf:type
    ((string= token "a")
     "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
    ;; Boolean literals
    ((string= token "true") t)
    ((string= token "false") nil)
    (t token))))

;;; ==========================================================================
;;; Turtle Export (simplified)
;;; ==========================================================================

(defun export-turtle (g)
  "Export graph as Turtle format string (simplified)."
  (export-ntriples g))  ; Fallback to N-Triples for now

;;; ==========================================================================
;;; N-Quads Import
;;; ==========================================================================

(defun import-nquads (g data)
  "Import N-Quads format (triples with optional graph name)."
  ;; N-Quads is N-Triples with an optional 4th element (graph name)
  ;; We import the triples and ignore the graph name for now
  (import-ntriples g data))
