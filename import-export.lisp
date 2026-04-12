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
                 (setf value (convert-typed-literal value type-uri)))))))
        ;; Language tag: "Alice"@en
        ((and (< pos len) (char= #\@ (char line pos)))
         (let ((end (or (position #\Space line :start pos) len)))
           (setf value (intern-literal value +rdf-langstring+
                                       (string-downcase (subseq line (1+ pos) end))))
           (setf pos end)))
        ;; Plain string
        (t (setf value (intern-literal value +xsd-string+))))
      (cons value pos))))

(defun convert-typed-literal (value type-uri)
  "Convert a typed literal string to an interned rdf-literal."
  (handler-case
      (cond
        ((search "integer" type-uri)
         (intern-literal (parse-integer value) type-uri))
        ((or (search "decimal" type-uri) (search "float" type-uri) (search "double" type-uri))
         (let ((n (read-from-string value)))
           (intern-literal (if (numberp n) n value) type-uri)))
        ((search "boolean" type-uri)
         (intern-literal (string= value "true") type-uri))
        (t (intern-literal value type-uri)))
    (error () (intern-literal value type-uri))))

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
    ((rdf-literal-p term)
     (let ((val (rdf-literal-value term))
           (dt (rdf-literal-datatype term))
           (lang (rdf-literal-language term)))
       (cond
         (lang (format nil "\"~A\"@~A" val lang))
         ((equal dt +xsd-string+) (format nil "\"~A\"" val))
         ((equal dt +xsd-boolean+) (format nil "\"~A\"^^<~A>" (if val "true" "false") dt))
         (t (format nil "\"~A\"^^<~A>" val dt)))))
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

;;; ==========================================================================
;;; Turtle Validation
;;; ==========================================================================

(defun validate-uri-token (tok)
  "Signal error if URI token contains invalid characters or escapes."
  (let ((uri (subseq tok 1 (1- (length tok)))))
    ;; No character escapes allowed in URIs (only \uXXXX and \UXXXXXXXX)
    (let ((i 0))
      (loop while (< i (length uri)) do
        (let ((c (char uri i)))
          (when (member c '(#\Space #\{ #\} #\| #\^ #\` #\< #\>))
            (error "Invalid character in URI: ~A" tok))
          (when (char= c #\\)
            (if (>= (1+ i) (length uri))
                (error "Trailing backslash in URI: ~A" tok)
                (let ((next (char uri (1+ i))))
                  (cond
                    ((char= next #\u)
                     (when (or (> (+ i 6) (length uri))
                               (not (every (lambda (c) (digit-char-p c 16))
                                           (coerce (subseq uri (+ i 2) (min (+ i 6) (length uri))) 'list))))
                       (error "Bad \\u escape in URI: ~A" tok))
                     ;; Check resolved char is valid in URI
                     (let ((code (parse-integer (subseq uri (+ i 2) (+ i 6)) :radix 16)))
                       (when (member (code-char code) '(#\Space #\< #\>))
                         (error "URI escape resolves to invalid character: ~A" tok)))
                     (incf i 5))
                    ((char= next #\U)
                     (when (or (> (+ i 10) (length uri))
                               (not (every (lambda (c) (digit-char-p c 16))
                                           (coerce (subseq uri (+ i 2) (min (+ i 10) (length uri))) 'list))))
                       (error "Bad \\U escape in URI: ~A" tok))
                     (let ((code (parse-integer (subseq uri (+ i 2) (+ i 10)) :radix 16)))
                       (when (member (code-char code) '(#\Space #\< #\>))
                         (error "URI escape resolves to invalid character: ~A" tok)))
                     (incf i 9))
                    (t (error "Only \\u and \\U escapes allowed in URIs: ~A" tok)))))))
        (incf i)))))

(defun validate-escape-sequences (str start end)
  "Signal error on invalid escape sequences in string content."
  (let ((i start))
    (loop while (< i end) do
      (cond
        ((char= #\\ (char str i))
         (if (>= (1+ i) end)
             (error "Trailing backslash in string")
             (let ((next (char str (1+ i))))
               (unless (member next '(#\t #\n #\r #\\ #\" #\' #\u #\U #\b #\f))
                 (error "Invalid escape sequence: \\~C" next))
               (when (char= next #\u)
                 (when (or (> (+ i 6) end)
                           (not (every (lambda (c) (digit-char-p c 16))
                                       (coerce (subseq str (+ i 2) (min (+ i 6) end)) 'list))))
                   (error "Bad \\u escape")))
               (when (char= next #\U)
                 (when (or (> (+ i 10) end)
                           (not (every (lambda (c) (digit-char-p c 16))
                                       (coerce (subseq str (+ i 2) (min (+ i 10) end)) 'list))))
                   (error "Bad \\U escape")))
               (incf i))))  ; skip past escaped char
        (t nil))
      (incf i))))

(defun validate-number-token (tok)
  "Signal error if token looks like a number but is malformed."
  ;; Reject double signs like +-1
  (when (and (>= (length tok) 2)
             (member (char tok 0) '(#\+ #\-))
             (member (char tok 1) '(#\+ #\-)))
    (error "Malformed numeric literal: ~A" tok))
  (unless (or (cl-ppcre:scan "^[+-]?[0-9]+$" tok)
              (cl-ppcre:scan "^[+-]?[0-9]*\\.[0-9]+$" tok)
              (cl-ppcre:scan "^[+-]?[0-9]+\\.[0-9]*$" tok)
              (cl-ppcre:scan "^[+-]?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)[eE][+-]?[0-9]+$" tok))
    (error "Malformed numeric literal: ~A" tok)))

(defun validate-lang-tag (tag)
  "Signal error if language tag is invalid."
  (unless (cl-ppcre:scan "^[a-zA-Z]+(-[a-zA-Z0-9]+)*$" tag)
    (error "Invalid language tag: @~A" tag)))

(defun validate-pname (token prefixes)
  "Signal error if prefixed name is invalid."
  (let ((colon-pos (position #\: token)))
    (when colon-pos
      (let ((prefix-part (subseq token 0 colon-pos))
            (local-part (subseq token (1+ colon-pos))))
        ;; Prefix must not start or end with dot
        (when (and (> (length prefix-part) 0)
                   (or (char= #\. (char prefix-part 0))
                       (char= #\. (char prefix-part (1- (length prefix-part))))))
          (error "Invalid prefix name: ~A" token))
        ;; Local name must not start with dash
        (when (and (> (length local-part) 0)
                   (char= #\- (char local-part 0)))
          (error "Local name cannot start with dash: ~A" token))
        ;; Reject ~ unescaped in local name (but \~ is valid escape)
        (let ((i 0))
          (loop while (< i (length local-part)) do
            (cond
              ((char= #\\ (char local-part i)) (incf i)) ; skip escaped char
              ((or (char= #\~ (char local-part i))
                   (char= #\^ (char local-part i)))
               (error "Unescaped special char in local name: ~A" token)))
            (incf i)))
        ;; Reject \\u in local name (not valid pname escape)
        (when (search "\\u" local-part)
          (error "\\u escape not valid in prefixed name: ~A" token))
        ;; Validate %-escapes: must be %HH
        (let ((i 0))
          (loop while (< i (length local-part)) do
            (when (char= #\% (char local-part i))
              (when (or (> (+ i 3) (length local-part))
                        (not (digit-char-p (char local-part (+ i 1)) 16))
                        (not (digit-char-p (char local-part (+ i 2)) 16)))
                (error "Bad %%-escape in local name: ~A" token)))
            (incf i)))
        ;; Check prefix is defined (only when prefixes table provided)
        (when prefixes
          (let ((prefix-key (concatenate 'string prefix-part ":")))
            (when (and (not (gethash prefix-key prefixes))
                       (not (string= prefix-part ""))
                       (not (string= prefix-part "_")))  ; _: is blank node, not prefix
              (error "Undefined prefix: ~A" prefix-key))))))))

(defun validate-prefix-decl (name)
  "Validate a prefix name in @prefix declaration."
  (let ((body (if (and (> (length name) 0)
                       (char= #\: (char name (1- (length name)))))
                  (subseq name 0 (1- (length name)))
                  name)))
    (when (> (length body) 0)
      (when (not (alpha-char-p (char body 0)))
        (error "Prefix name must start with letter: ~A" name))
      (when (char= #\. (char body (1- (length body))))
        (error "Prefix name cannot end with dot: ~A" name)))))

;;; ==========================================================================
;;; Turtle Import
;;; ==========================================================================

(defun import-turtle (g data &key (bnode-counter-start 0) graph-name)
  "Import Turtle format string into graph G.
Tokenizes the entire input then processes token stream."
  (let ((prefixes (make-hash-table :test 'equal))
        (tokens (turtle-tokenize data)))
    (turtle-parse-tokens g tokens prefixes bnode-counter-start graph-name)))

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
              ;; Punctuation (. is only punctuation if followed by whitespace/EOF/newline)
              ((member ch '(#\; #\, #\( #\) #\[ #\]))
               (push (string ch) tokens)
               (incf pos))
              ((char= ch #\.)
               (if (or (>= (1+ pos) len)
                       (member (char data (1+ pos)) '(#\Space #\Tab #\Newline #\Return)))
                   (progn (push (string ch) tokens) (incf pos))
                   ;; Dot is part of a token (e.g. prefixed name with dots)
                   (let ((start pos))
                     (loop while (and (< pos len)
                                      (not (member (char data pos)
                                                   '(#\Space #\Tab #\Newline #\Return
                                                     #\; #\,))))
                           do (if (and (char= (char data pos) #\.)
                                       (or (>= (1+ pos) len)
                                           (member (char data (1+ pos))
                                                   '(#\Space #\Tab #\Newline #\Return))))
                                  (return)
                                  (incf pos)))
                     (when (> pos start)
                       (push (subseq data start pos) tokens)))))
              ;; URI <...>
              ((char= ch #\<)
               (let ((end (position #\> data :start (1+ pos))))
                 (if end
                     (let ((tok (subseq data pos (1+ end))))
                       (validate-uri-token tok)
                       (push tok tokens)
                       (setf pos (1+ end)))
                     (error "Unterminated URI"))))
              ;; String — check for long literals first (""" or ''')
              ((char= ch #\")
               (let ((start pos))
                 (if (and (< (+ pos 2) len)
                          (char= #\" (char data (1+ pos)))
                          (char= #\" (char data (+ pos 2))))
                     ;; Long literal """..."""
                     (progn
                       (incf pos 3) ; skip opening """
                       (let ((found nil))
                         (loop while (< pos len) do
                           (if (and (<= (+ pos 2) len)
                                    (char= #\" (char data pos))
                                    (< (1+ pos) len)
                                    (char= #\" (char data (1+ pos)))
                                    (< (+ pos 2) len)
                                    (char= #\" (char data (+ pos 2))))
                               (progn (incf pos 3) (setf found t) (return))
                               (progn
                                 (when (and (< pos len) (char= #\\ (char data pos)))
                                   (incf pos))
                                 (incf pos))))
                         (unless found
                           (error "Unterminated long string literal"))))
                     ;; Short literal "..."
                     (progn
                       (incf pos) ; skip opening "
                       (loop while (and (< pos len) (char/= #\" (char data pos)))
                             do (when (char= #\\ (char data pos)) (incf pos))
                                (incf pos))
                       (if (< pos len)
                           (incf pos) ; skip closing "
                           (error "Unclosed string literal in Turtle input"))))
                 ;; Validate escape sequences in the string body
                 (let ((q-len (if (and (>= (- pos start) 6)
                                       (char= #\" (char data (1+ start)))
                                       (char= #\" (char data (+ start 2))))
                                  3 1)))
                   (validate-escape-sequences data (+ start q-len) (- pos q-len)))
                 ;; Check for ^^type or @lang suffix
                 (when (and (< (1+ pos) len)
                            (char= #\^ (char data pos))
                            (char= #\^ (char data (1+ pos))))
                   (incf pos 2)
                   (if (and (< pos len) (char= #\< (char data pos)))
                       (let ((end (position #\> data :start pos)))
                         (when end (setf pos (1+ end))))
                       (loop while (and (< pos len)
                                        (not (member (char data pos)
                                                     '(#\Space #\Tab #\Newline #\Return
                                                       #\. #\; #\, #\( #\) #\[ #\]))))
                             do (incf pos))))
                 (when (and (< pos len) (char= #\@ (char data pos)))
                   ;; Check not also ^^
                   (let ((lang-start (1+ pos)))
                     (loop while (and (< pos len)
                                      (not (member (char data pos)
                                                   '(#\Space #\Tab #\Newline #\Return
                                                     #\. #\; #\,))))
                           do (incf pos))
                     (validate-lang-tag (subseq data lang-start pos))))
                 ;; Reject both @lang and ^^type
                 (let ((tok (subseq data start pos)))
                   (when (and (search "@" tok) (search "^^" tok)
                              (< (position #\@ tok :start 1) (search "^^" tok)))
                     (error "Literal cannot have both language tag and datatype: ~A" tok))
                   (push tok tokens))))
              ;; Single-quoted strings (also check for long ''')
              ((char= ch #\')
               (let ((start pos))
                 (if (and (< (+ pos 2) len)
                          (char= #\' (char data (1+ pos)))
                          (char= #\' (char data (+ pos 2))))
                     ;; Long literal '''...'''
                     (progn
                       (incf pos 3)
                       (let ((found nil))
                         (loop while (< pos len) do
                           (if (and (<= (+ pos 2) len)
                                    (char= #\' (char data pos))
                                    (< (1+ pos) len)
                                    (char= #\' (char data (1+ pos)))
                                    (< (+ pos 2) len)
                                    (char= #\' (char data (+ pos 2))))
                               (progn (incf pos 3) (setf found t) (return))
                               (progn
                                 (when (and (< pos len) (char= #\\ (char data pos)))
                                   (incf pos))
                                 (incf pos))))
                         (unless found
                           (error "Unterminated long string literal"))))
                     ;; Short literal '...'
                     (progn
                       (incf pos)
                       (loop while (and (< pos len) (char/= #\' (char data pos)))
                             do (when (char= #\\ (char data pos)) (incf pos))
                                (incf pos))
                       (if (< pos len)
                           (incf pos)
                           (error "Unclosed string literal"))))
                 ;; Validate escapes
                 (let ((q-len (if (and (>= (- pos start) 6)
                                       (char= #\' (char data (1+ start)))
                                       (char= #\' (char data (+ start 2))))
                                  3 1)))
                   (validate-escape-sequences data (+ start q-len) (- pos q-len)))
                 ;; Check for ^^type or @lang suffix
                 (when (and (< (1+ pos) len)
                            (char= #\^ (char data pos))
                            (char= #\^ (char data (1+ pos))))
                   (incf pos 2)
                   (if (and (< pos len) (char= #\< (char data pos)))
                       (let ((end (position #\> data :start pos)))
                         (when end (setf pos (1+ end))))
                       (loop while (and (< pos len)
                                        (not (member (char data pos)
                                                     '(#\Space #\Tab #\Newline #\Return
                                                       #\. #\; #\, #\( #\) #\[ #\]))))
                             do (incf pos))))
                 (when (and (< pos len) (char= #\@ (char data pos)))
                   (let ((lang-start (1+ pos)))
                     (loop while (and (< pos len)
                                      (not (member (char data pos)
                                                   '(#\Space #\Tab #\Newline #\Return
                                                     #\. #\; #\,))))
                           do (incf pos))
                     (validate-lang-tag (subseq data lang-start pos))))
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
                 ;; Read including dots (don't stop at dot)
                 (loop while (and (< pos len)
                                  (not (member (char data pos)
                                               '(#\Space #\Tab #\Newline #\Return
                                                 #\; #\, #\( #\) #\[ #\]))))
                       do (incf pos))
                 (let ((tok (subseq data start pos)))
                   ;; Check if label ends with dot — invalid per W3C
                   (when (and (> (length tok) 2)
                              (char= #\. (char tok (1- (length tok)))))
                     (error "Blank node label cannot end with dot: ~A" tok))
                   (push tok tokens))))
              ;; Other token (prefixed name, number, etc.)
              (t
               (let ((start pos))
                 (loop while (< pos len) do
                   (let ((c (char data pos)))
                     (cond
                       ((member c '(#\Space #\Tab #\Newline #\Return
                                    #\; #\, #\( #\) #\[ #\]))
                        (return))
                       ;; Backslash escape: skip next char
                       ((char= c #\\)
                        (incf pos)
                        (when (< pos len) (incf pos)))
                       ;; Dot: stop only if followed by ws/punct/EOF
                       ((char= c #\.)
                        (if (or (>= (1+ pos) len)
                                (member (char data (1+ pos))
                                        '(#\Space #\Tab #\Newline #\Return
                                          #\; #\, #\( #\) #\[ #\])))
                            (return)
                            (incf pos)))
                       (t (incf pos)))))
                 (when (> pos start)
                   (push (subseq data start pos) tokens)))))))))
    (nreverse tokens)))

(defun parse-bnode-contents (g toks prefixes anon-counter bnode)
  "Parse predicate-object pairs inside a blank node [...]. Returns (toks anon-counter)."
  (loop while (and toks (not (string= "]" (car toks)))) do
    (let ((bp (turtle-resolve (pop toks) prefixes))
          (bo nil))
      (cond
        ;; Object is a collection
        ((and toks (string= "(" (car toks)))
         (pop toks)
         (let ((r (parse-collection g toks prefixes anon-counter)))
           (setf toks (first r) anon-counter (second r) bo (third r))))
        ;; Object is a blank node
        ((and toks (string= "[" (car toks)))
         (pop toks)
         (let ((inner (format nil "_:anon~A" (incf anon-counter))))
           (setf bo inner)
           (if (and toks (string= "]" (car toks)))
               (pop toks)
               (progn
                 (multiple-value-bind (new-toks new-ac)
                     (parse-bnode-contents g toks prefixes anon-counter inner)
                   (setf toks new-toks anon-counter new-ac))
                 (when (and toks (string= "]" (car toks))) (pop toks))))))
        ;; Simple object
        (toks (setf bo (turtle-resolve (pop toks) prefixes))))
      (when (and bp bo) (add-triple g bnode bp bo))
      ;; Handle , for multiple objects
      (loop while (and toks (string= "," (car toks))) do
        (pop toks)
        (let ((extra nil))
          (cond
            ((and toks (string= "(" (car toks)))
             (pop toks)
             (let ((r (parse-collection g toks prefixes anon-counter)))
               (setf toks (first r) anon-counter (second r) extra (third r))))
            ((and toks (string= "[" (car toks)))
             (pop toks)
             (let ((inner (format nil "_:anon~A" (incf anon-counter))))
               (setf extra inner)
               (if (and toks (string= "]" (car toks)))
                   (pop toks)
                   (progn
                     (multiple-value-bind (new-toks new-ac)
                         (parse-bnode-contents g toks prefixes anon-counter inner)
                       (setf toks new-toks anon-counter new-ac))
                     (when (and toks (string= "]" (car toks))) (pop toks))))))
            (toks (setf extra (turtle-resolve (pop toks) prefixes))))
          (when (and bp extra) (add-triple g bnode bp extra))))
      (when (and toks (string= ";" (car toks))) (pop toks))))
  (values toks anon-counter))

(defun parse-collection (g toks prefixes anon-counter)
  "Parse an RDF collection from token stream (after opening paren consumed).
Returns (remaining-toks anon-counter list-head-node)."
  (let* ((rdf-first "http://www.w3.org/1999/02/22-rdf-syntax-ns#first")
         (rdf-rest "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest")
         (rdf-nil "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")
         (head nil)
         (prev nil))
    (loop while (and toks (not (string= ")" (car toks)))) do
      (let ((node (format nil "_:list~A" (incf anon-counter)))
            (item nil))
        (unless head (setf head node))
        (when prev (add-triple g prev rdf-rest node))
        (cond
          ;; Nested collection
          ((string= "(" (car toks))
           (pop toks)
           (let ((result (parse-collection g toks prefixes anon-counter)))
             (setf toks (first result) anon-counter (second result) item (third result))))
          ;; Blank node property list
          ((string= "[" (car toks))
           (pop toks)
           (let ((bnode (format nil "_:anon~A" (incf anon-counter))))
             (setf item bnode)
             (if (and toks (string= "]" (car toks)))
                 (pop toks)
                 (progn
                   (multiple-value-bind (new-toks new-ac)
                       (parse-bnode-contents g toks prefixes anon-counter bnode)
                     (setf toks new-toks anon-counter new-ac))
                   (when (and toks (string= "]" (car toks))) (pop toks))))))
          ;; Simple value
          (t (setf item (turtle-resolve (pop toks) prefixes))))
        (add-triple g node rdf-first item)
        (setf prev node)))
    (when prev (add-triple g prev rdf-rest rdf-nil))
    (unless head (setf head rdf-nil))
    (when (and toks (string= ")" (car toks))) (pop toks))
    (list toks anon-counter head)))

(defun turtle-parse-tokens (g tokens prefixes &optional (anon-start 0) graph-name)
  "Parse a token stream into triples. Uses a context stack for nested blank nodes."
  (let ((toks tokens)
        (subject nil)
        (predicate nil)
        (base-uri nil)
        (anon-counter anon-start)
        (had-predicate nil)
        (expect-punct nil)
        (bracket-depth 0)
        (context-stack nil))
    ;; Reject N3/TriG tokens at top level
    (dolist (tok tokens)
      (when (or (string= tok "=") (string= tok "=>") (string= tok "<=")
                (and (> (length tok) 0) (char= #\{ (char tok 0)))
                (string-equal tok "@forSome") (string-equal tok "@forAll")
                (string-equal tok "@keywords")
                (string= tok "is") (string= tok "of"))
        (error "N3/TriG syntax not valid in Turtle: ~A" tok)))
    (loop while toks do
      (let ((tok (car toks)))
        (cond
          ;; @prefix
          ((string-equal tok "@prefix")
           (unless (string= tok "@prefix")
             (error "@prefix must be lowercase: ~A" tok))
           (pop toks)
           (let ((prefix-name (pop toks))
                 (uri (pop toks)))
             (unless (and prefix-name (> (length prefix-name) 0)
                          (char= #\: (char prefix-name (1- (length prefix-name)))))
               (error "Malformed @prefix declaration"))
             (unless (and uri (> (length uri) 1) (char= #\< (char uri 0)))
               (error "Malformed @prefix: missing URI"))
             (validate-prefix-decl prefix-name)
             (setf (gethash prefix-name prefixes)
                   (subseq uri 1 (1- (length uri))))
             (when (and toks (string= "." (car toks)))
               (pop toks))))
          ;; @base
          ((string-equal tok "@base")
           (unless (string= tok "@base")
             (error "@base must be lowercase: ~A" tok))
           (pop toks)
           (let ((uri (pop toks)))
             (unless (and uri (> (length uri) 1) (char= #\< (char uri 0)))
               (error "Malformed @base: missing URI"))
             (setf base-uri (subseq uri 1 (1- (length uri))))
             (when (and toks (string= "." (car toks)))
               (pop toks))))
          ;; SPARQL-style PREFIX (no dot after)
          ((string-equal tok "PREFIX")
           (pop toks)
           (let ((prefix-name (pop toks))
                 (uri (pop toks)))
             (when (and prefix-name uri)
               (setf (gethash prefix-name prefixes)
                     (subseq uri 1 (1- (length uri)))))))
          ;; SPARQL-style BASE (no dot after)
          ((string-equal tok "BASE")
           (pop toks)
           (let ((uri (pop toks)))
             (when uri
               (setf base-uri (subseq uri 1 (1- (length uri)))))
             ;; SPARQL BASE must NOT have trailing dot
             (when (and toks (string= "." (car toks)))
               (error "SPARQL BASE must not end with dot"))))
          ;; "."  — end of statement
          ((string= tok ".")
           (pop toks)
           ;; Dot is not valid inside blank node property lists
           (when (> bracket-depth 0)
             (error "Unexpected dot inside blank node property list"))
           (when (and (null subject) (null predicate) (not had-predicate))
             (error "Unexpected dot without statement"))
           (when (and subject (null predicate) (not had-predicate))
             (error "Incomplete statement: subject without predicate"))
           (setf subject nil predicate nil had-predicate nil expect-punct nil))
          ;; ";" — same subject, new predicate
          ((string= tok ";")
           (pop toks)
           (setf predicate nil had-predicate t expect-punct nil)
           ;; Trailing ; without next predicate — check for dot or EOF
           (when (or (null toks) (string= "." (car toks)))
             ;; Trailing ; before . is allowed in Turtle
             nil))
          ;; "]" — closing blank node, restore context
          ((string= tok "]")
           (pop toks)
           (decf bracket-depth)
           (let ((bnode subject))
             (if context-stack
                 (let ((ctx (pop context-stack)))
                   (setf subject (first ctx)
                         predicate (second ctx)
                         had-predicate (third ctx))
                   ;; The blank node fills the role in the outer context
                   (cond
                     ;; Was in subject position
                     ((and (null subject) (null predicate))
                      (setf subject bnode had-predicate t expect-punct nil))
                     ;; Was in object position (subject+predicate were set)
                     ((and subject predicate)
                      (add-triple g subject predicate bnode :graph-name graph-name)
                      (setf expect-punct t))
                     ;; Subject set but no predicate — blank node is done as subject
                     (t (setf subject bnode expect-punct nil))))
                 ;; No context to restore — top-level blank node subject
                 (setf expect-punct nil))))
          ;; ")" — stray close paren (collections handle their own)
          ((string= tok ")")
           (pop toks)
           (decf bracket-depth))
          ;; "[" — blank node property list
          ((string= tok "[")
           (pop toks)
           (incf bracket-depth)
           (setf expect-punct nil)
           (let ((bnode (format nil "_:anon~A" (incf anon-counter))))
             (cond
               ;; [] empty blank node
               ((and toks (string= "]" (car toks)))
                (pop toks)
                (decf bracket-depth)
                (cond
                  ((null subject) (setf subject bnode))
                  ((null predicate) (error "Blank nodes cannot be predicates"))
                  (t (add-triple g subject predicate bnode :graph-name graph-name)
                     (setf expect-punct t))))
               ;; Non-empty: push context, parse contents with bnode as subject
               (t
                (push (list subject predicate had-predicate) context-stack)
                (setf subject bnode predicate nil had-predicate nil expect-punct nil)))))
          ;; "(" — RDF collection
          ((string= tok "(")
           (pop toks)
           (setf expect-punct nil)
           (let ((result (parse-collection g toks prefixes anon-counter)))
             (setf toks (first result)
                   anon-counter (second result))
             (let ((list-node (third result)))
               (cond
                 ((null subject) (setf subject list-node))
                 ((null predicate) (error "Collection cannot be a predicate"))
                 (t (add-triple g subject predicate list-node :graph-name graph-name)
                    (setf expect-punct t))))))
          ;; "," — same subject and predicate, new object
          ((string= tok ",")
           (pop toks)
           (setf expect-punct nil)
           (when (and subject predicate toks)
             (cond
               ;; Blank node as object
               ((string= (car toks) "[")
                (pop toks)
                (let ((bnode (format nil "_:anon~A" (incf anon-counter))))
                  (multiple-value-bind (new-toks new-ac)
                      (parse-bnode-contents g toks prefixes anon-counter bnode)
                    (setf toks new-toks anon-counter new-ac))
                  (add-triple g subject predicate bnode :graph-name graph-name)
                  (setf expect-punct t)))
               ;; Collection as object
               ((string= (car toks) "(")
                (pop toks)
                (let ((result (parse-collection g toks prefixes anon-counter)))
                  (setf toks (first result) anon-counter (second result))
                  (add-triple g subject predicate (third result) :graph-name graph-name)
                  (setf expect-punct t)))
               ;; Regular object
               (t (let ((obj (turtle-resolve (pop toks) prefixes)))
                    (add-triple g subject predicate obj :graph-name graph-name)
                    (setf expect-punct t))))))
          ;; Regular token
          (t
           (when (and expect-punct (= 0 bracket-depth))
             (error "Expected . ; or , after object, got: ~A" tok))
           (cond
             ;; Need subject
             ((null subject)
              (let ((tok (pop toks)))
                ;; Reject literals as subjects
                (when (and (> (length tok) 0)
                           (or (char= #\" (char tok 0))
                               (char= #\' (char tok 0))))
                  (error "Literals cannot be subjects: ~A" tok))
                ;; Reject bare keywords as subjects
                (when (member tok '("true" "false" "a") :test #'string=)
                  (when (not (position #\: tok))
                    (error "Keywords cannot be subjects: ~A" tok)))
                (setf subject (turtle-resolve tok prefixes))))
             ;; Need predicate
             ((null predicate)
              (let ((tok (pop toks)))
                ;; Reject literals as predicates (only at top level)
                (when (and (= 0 bracket-depth)
                           (> (length tok) 0)
                           (or (char= #\" (char tok 0))
                               (char= #\' (char tok 0))))
                  (error "Literals cannot be predicates: ~A" tok))
                ;; Reject blank nodes as predicates
                (when (or (and (> (length tok) 1)
                               (char= #\_ (char tok 0))
                               (char= #\: (char tok 1)))
                          (string= tok "["))
                  (error "Blank nodes cannot be predicates: ~A" tok))
                ;; Reject bare keywords as predicates (except 'a')
                (when (and (member tok '("true" "false") :test #'string=)
                           (not (position #\: tok)))
                  (error "Keywords cannot be predicates: ~A" tok))
                ;; Reject uppercase A
                (when (string= tok "A")
                  (error "'a' shorthand must be lowercase"))
                (setf predicate (turtle-resolve tok prefixes)
                      had-predicate t)))
             ;; Have both — this is the object
             (t
              (let* ((obj-tok (pop toks))
                     (obj (turtle-resolve obj-tok prefixes)))
                ;; 'a' is only valid as predicate at top level
                (when (and (string= obj-tok "a") (= 0 bracket-depth))
                  (error "'a' is only valid as predicate, not object"))
                (add-triple g subject predicate obj :graph-name graph-name)
                (setf expect-punct t)))))))
      )
    ;; If we have a subject but no dot was seen, that's an error
    (when subject
      (error "Unterminated triple statement"))))
(defun turtle-resolve (token prefixes)
  "Resolve a Turtle token to a value."
  (flet ((expand-prefix (term)
           (let ((colon-pos (position #\: term)))
             (if colon-pos
                 (let* ((prefix (concatenate 'string (subseq term 0 (1+ colon-pos))))
                        (local (subseq term (1+ colon-pos)))
                        (base (gethash prefix prefixes)))
                   (if base
                       (concatenate 'string base local)
                       (error "Undefined prefix: ~A" prefix)))
                 term))))
    (cond
    ;; URI: <http://...>
    ((and (> (length token) 1)
          (char= #\< (char token 0)))
     (subseq token 1 (1- (length token))))
    ;; Quoted string (possibly with type/lang)
    ((and (> (length token) 0)
          (or (char= #\" (char token 0))
              (char= #\' (char token 0))))
     (let* ((long-p (and (>= (length token) 6)
                         (let ((q (char token 0)))
                           (and (char= q (char token 1))
                                (char= q (char token 2))))))
            (delim-len (if long-p 3 1))
            (q (char token 0))
            ;; Find closing delimiter
            (end (if long-p
                     (search (make-string 3 :initial-element q) token :start2 3)
                     (position q token :start 1))))
       (if end
           (let ((str (subseq token delim-len end))
                 (rest (subseq token (+ end delim-len))))
             (cond
               ((and (>= (length rest) 2)
                     (string= "^^" (subseq rest 0 2)))
                (let* ((type-tok (subseq rest 2))
                       (type-uri (if (and (> (length type-tok) 0) (char= #\< (char type-tok 0)))
                                     (subseq type-tok 1 (1- (length type-tok)))
                                     (expand-prefix type-tok))))
                  (convert-typed-literal str type-uri)))
               ;; Language tag: @en, @fr-FR, etc.
               ((and (> (length rest) 0) (char= #\@ (char rest 0)))
                (intern-literal str +rdf-langstring+ (string-downcase (subseq rest 1))))
               (t (intern-literal str +xsd-string+))))
           token)))
    ;; Number
    ((and (> (length token) 0)
          (or (digit-char-p (char token 0))
              (and (> (length token) 1)
                   (member (char token 0) '(#\+ #\-))
                   (or (digit-char-p (char token 1))
                       (char= #\. (char token 1))
                       (member (char token 1) '(#\+ #\-))))))
     (validate-number-token token)
     (let ((val (ignore-errors (read-from-string token))))
       (if (numberp val)
           (intern-literal val (cond ((integerp val) +xsd-integer+)
                                     ((typep val 'double-float) +xsd-double+)
                                     (t +xsd-decimal+)))
           token)))
    ;; Blank node _:...
    ((and (> (length token) 1)
          (char= #\_ (char token 0))
          (char= #\: (char token 1)))
     token)
    ;; Prefixed name
    ((position #\: token)
     (validate-pname token prefixes)
     (expand-prefix token))
    ;; 'a' shorthand for rdf:type
    ((string= token "a")
     "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
    ;; Boolean literals
    ((string= token "true") t)
    ((string= token "false") nil)
    ;; Reject N3 keywords
    ((member token '("{" "}" "=" "=>" "<=" "is" "of" "@forSome" "@forAll" "@keywords")
             :test #'string=)
     (error "N3 syntax not valid in Turtle: ~A" token))
    (t token))))

;;; ==========================================================================
;;; Turtle Export (simplified)
;;; ==========================================================================

(defun export-turtle (g)
  "Export graph as Turtle format string with prefix detection and shorthand."
  (let ((prefixes (detect-prefixes g))
        (rdf-type "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
    (with-output-to-string (s)
      ;; Emit prefix declarations
      (maphash (lambda (prefix uri)
                 (format s "@prefix ~A: <~A> .~%" prefix uri))
               prefixes)
      (when (> (hash-table-count prefixes) 0)
        (format s "~%"))
      ;; Group triples by subject
      (let ((by-subject (make-hash-table :test 'equal)))
        (dolist (tr (get-triples g))
          (push tr (gethash (triple-subject tr) by-subject)))
        ;; Emit each subject group
        (maphash
         (lambda (subj triples)
           ;; Group by predicate within subject
           (let ((by-pred (make-hash-table :test 'equal)))
             (dolist (tr triples)
               (push (triple-object tr) (gethash (triple-predicate tr) by-pred)))
             ;; Emit subject
             (format s "~A" (shorten-uri subj prefixes))
             (let ((first-pred t))
               (maphash
                (lambda (pred objects)
                  (if first-pred
                      (setf first-pred nil)
                      (format s " ;~%   "))
                  ;; Use 'a' for rdf:type
                  (format s " ~A"
                          (if (equal pred rdf-type) "a"
                              (shorten-uri pred prefixes)))
                  ;; Emit objects with comma shorthand
                  (let ((first-obj t))
                    (dolist (obj objects)
                      (if first-obj
                          (setf first-obj nil)
                          (format s ","))
                      (format s " ~A" (format-turtle-object obj prefixes)))))
                by-pred))
             (format s " .~%")))
         by-subject)))))

(defun detect-prefixes (g)
  "Detect common URI prefixes in the graph."
  (let ((uri-counts (make-hash-table :test 'equal))
        (prefixes (make-hash-table :test 'equal))
        (prefix-id 0))
    ;; Count URI bases
    (dolist (tr (get-triples g))
      (dolist (term (list (triple-subject tr) (triple-predicate tr) (triple-object tr)))
        (when (and (stringp term) (search "://" term))
          (let ((base (uri-base term)))
            (when base (incf (gethash base uri-counts 0)))))))
    ;; Assign prefixes to frequently used bases
    (maphash (lambda (base count)
               (when (>= count 2)
                 (let ((name (or (known-prefix base)
                                 (format nil "ns~A" (incf prefix-id)))))
                   (setf (gethash name prefixes) base))))
             uri-counts)
    prefixes))

(defun uri-base (uri)
  "Extract the base of a URI (up to last / or #)."
  (let ((hash-pos (position #\# uri :from-end t))
        (slash-pos (position #\/ uri :from-end t)))
    (let ((split (or hash-pos slash-pos)))
      (when (and split (> split 8))  ; skip http://
        (subseq uri 0 (1+ split))))))

(defun known-prefix (base)
  "Return a well-known prefix name for a URI base, or nil."
  (cond
    ((search "www.w3.org/1999/02/22-rdf-syntax-ns#" base) "rdf")
    ((search "www.w3.org/2000/01/rdf-schema#" base) "rdfs")
    ((search "www.w3.org/2002/07/owl#" base) "owl")
    ((search "www.w3.org/2001/XMLSchema#" base) "xsd")
    ((search "xmlns.com/foaf/0.1/" base) "foaf")
    ((search "purl.org/dc/terms/" base) "dct")
    ((search "purl.org/dc/elements/1.1/" base) "dc")
    ((search "schema.org/" base) "schema")
    ((search "example.org/" base) "ex")
    ((search "example.com/" base) "ex")
    (t nil)))

(defun shorten-uri (uri prefixes)
  "Shorten a URI using known prefixes, or wrap in <...>."
  (when (not (stringp uri))
    (return-from shorten-uri (format nil "~A" uri)))
  (maphash (lambda (prefix base)
             (when (and (>= (length uri) (length base))
                        (string= base (subseq uri 0 (length base))))
               (return-from shorten-uri
                 (format nil "~A:~A" prefix (subseq uri (length base))))))
           prefixes)
  (if (search "://" uri)
      (format nil "<~A>" uri)
      (format nil "~A" uri)))

(defun format-turtle-object (obj prefixes)
  "Format an object for Turtle output."
  (cond
    ((stringp obj)
     (if (search "://" obj)
         (shorten-uri obj prefixes)
         (format nil "\"~A\"" obj)))
    ((integerp obj) (format nil "~A" obj))
    ((floatp obj) (format nil "~A" obj))
    ((eq obj t) "true")
    ((null obj) "false")
    (t (shorten-uri (princ-to-string obj) prefixes))))

;;; ==========================================================================
;;; N-Quads Import
;;; ==========================================================================

(defun import-nquads (g data)
  "Import N-Quads format (triples with optional graph name)."
  (with-input-from-string (s data)
    (loop for line = (read-line s nil nil)
          while line do
          (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
            (when (and (> (length trimmed) 0)
                       (char/= #\# (char trimmed 0)))
              (let ((tokens (tokenize-ntriple trimmed)))
                (when (>= (length tokens) 3)
                  (if (>= (length tokens) 4)
                      (add-quad g (first tokens) (second tokens) (third tokens) (fourth tokens))
                      (add-triple g (first tokens) (second tokens) (third tokens))))))))))


;;; ==========================================================================
;;; N-Quads Export
;;; ==========================================================================

(defun export-nquads (g)
  "Export graph as N-Quads format string, including graph names."
  (with-output-to-string (s)
    ;; Triples with graph names
    (maphash (lambda (graph-name triples)
               (dolist (tr triples)
                 (format s "~A ~A ~A ~A .~%"
                         (format-nt-term (triple-subject tr))
                         (format-nt-term (triple-predicate tr))
                         (format-nt-term (triple-object tr))
                         (format-nt-term graph-name))))
             (graph-graph-index g))
    ;; Triples without graph names
    (dolist (tr (get-triples g))
      (unless (triple-graph tr)
        (format s "~A ~A ~A .~%"
                (format-nt-term (triple-subject tr))
                (format-nt-term (triple-predicate tr))
                (format-nt-term (triple-object tr)))))))
