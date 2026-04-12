;;;; run-sparql-w3c.lisp — W3C SPARQL 1.1 query conformance tests

(require :asdf)
(asdf:load-system :ariadne)
(in-package :ariadne)

(defvar *sbase* (merge-pathnames "test-data/rdf-tests/sparql/sparql11/"
                                 (asdf:system-source-directory :ariadne)))

(defun slurp (path)
  (uiop:read-file-string path))

(defun parse-srx (path)
  "Parse .srx into list of alists, or T/NIL for ASK."
  (let ((xml (slurp path)) (results nil) (bindings nil) (var nil))
    (dolist (line (cl-ppcre:split "\\n" xml))
      (let ((s (string-trim '(#\Space #\Tab #\Return) line)))
        (cond
          ((search "<boolean>true</boolean>" s) (return-from parse-srx t))
          ((search "<boolean>false</boolean>" s) (return-from parse-srx nil))
          (t
           (when (cl-ppcre:scan "<binding name=\"([^\"]+)\"" s)
             (setf var (aref (nth-value 1 (cl-ppcre:scan-to-strings "<binding name=\"([^\"]+)\"" s)) 0)))
           (cond
             ((cl-ppcre:scan "<uri>([^<]+)</uri>" s)
              (push (cons var (aref (nth-value 1 (cl-ppcre:scan-to-strings "<uri>([^<]+)</uri>" s)) 0)) bindings))
             ((cl-ppcre:scan "<literal[^>]*>([^<]*)</literal>" s)
              (push (cons var (aref (nth-value 1 (cl-ppcre:scan-to-strings "<literal[^>]*>([^<]*)</literal>" s)) 0)) bindings))
             ((search "<unbound/>" s) (push (cons var nil) bindings)))
           (when (search "</result>" s)
             (push (nreverse bindings) results) (setf bindings nil))))))
    (nreverse results)))

(defun parse-srj (path)
  "Parse .srj (JSON) into list of alists, or T/NIL for ASK."
  (let ((json (slurp path)) (results nil))
    ;; ASK results
    (when (cl-ppcre:scan "\"boolean\"\\s*:\\s*true" json) (return-from parse-srj t))
    (when (cl-ppcre:scan "\"boolean\"\\s*:\\s*false" json) (return-from parse-srj nil))
    ;; SELECT results: extract each binding object
    (cl-ppcre:do-matches-as-strings (block "\\{[^{}]*\"type\"[^{}]*\\}" json)
      (let ((bindings nil))
        (cl-ppcre:do-register-groups (name val)
            ("\"(\\w+)\"\\s*:\\s*\\{[^}]*\"value\"\\s*:\\s*\"([^\"]*)\""  block)
          (push (cons name val) bindings))
        (when bindings (push (nreverse bindings) results))))
    (nreverse results)))

(defun parse-expected-graph (path)
  "Parse .ttl result into a graph and return triple count."
  (let ((g (make-graph)))
    (import-turtle g (slurp path))
    g))

(defun parse-expected (dir rf)
  "Parse expected result file in any supported format."
  (let ((path (merge-pathnames rf dir))
        (rf-str (lit-val rf)))
    (cond
      ((cl-ppcre:scan "\\.srx$" rf-str) (values (parse-srx path) :srx))
      ((cl-ppcre:scan "\\.srj$" rf-str) (values (parse-srj path) :srj))
      ((cl-ppcre:scan "\\.ttl$" rf-str) (values (parse-expected-graph path) :ttl))
      (t (values nil :unknown)))))

(defun run-one-eval-test (dir qf df rf &optional graph-data-files)
  "Run one evaluation test with 5s timeout. Returns :pass, :fail, or :skip."
  (multiple-value-bind (expected fmt) (parse-expected dir rf)
    (when (eq fmt :unknown) (return-from run-one-eval-test :skip))
    (handler-case
        (#+sbcl sb-ext:with-timeout #+sbcl 5
         #-sbcl progn
          (let ((g (make-graph)))
            (when df
              (let ((path (merge-pathnames df dir)))
                (if (cl-ppcre:scan "\\.rdf$" df)
                    (import-rdf-xml g (slurp path))
                    (import-turtle g (slurp path)))))
            (dolist (gf graph-data-files)
              (let ((gpath (merge-pathnames gf dir)))
                (when (probe-file gpath)
                  (import-turtle g (slurp gpath) :graph-name (namestring gf)))))
            (let ((actual (sparql g (slurp (merge-pathnames qf dir)))))
              (cond
                ;; ASK result
                ((member expected '(t nil))
                 (if (eq (not (not actual)) expected) :pass :fail))
                ;; CONSTRUCT/graph result — compare triple counts
                ((eq fmt :ttl)
                 (if (and (listp actual)
                          (= (length actual) (graph-count expected)))
                     :pass :fail))
                ;; SELECT result — compare row counts
                (t (if (= (length actual) (length expected)) :pass :fail))))))
      (error () :fail)
      #+sbcl (sb-ext:timeout () :fail))))

(defun run-category (cat)
  (let* ((dir (merge-pathnames (format nil "~A/" cat) *sbase*))
         (mf (merge-pathnames "manifest.ttl" dir))
         (pass 0) (fail 0) (skip 0))
    (unless (probe-file mf) (return-from run-category (values 0 0 0)))
    (let ((mg (make-graph)))
      (handler-case (import-turtle mg (slurp mf)) (error () (return-from run-category (values 0 0 1))))
      (dolist (tr (get-triples mg :predicate "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
        (let ((subj (triple-subject tr))
              (typ (triple-object tr)))
          (handler-case
              (let ((name (or (lit-val (triple-object (first (get-triples mg :subject subj
                                :predicate "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#name"))))
                              (subseq subj (1+ (or (position #\# subj :from-end t) -1))))))
                (cond
                  ((equal typ "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#QueryEvaluationTest")
                   (let* ((act (triple-object (first (get-triples mg :subject subj
                                 :predicate "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#action"))))
                          (qf (when act (triple-object (first (get-triples mg :subject act
                                 :predicate "http://www.w3.org/2001/sw/DataAccess/tests/test-query#query")))))
                          (df (when act (triple-object (first (get-triples mg :subject act
                                 :predicate "http://www.w3.org/2001/sw/DataAccess/tests/test-query#data")))))
                          (gfs (when act (mapcar #'triple-object
                                                 (get-triples mg :subject act
                                                   :predicate "http://www.w3.org/2001/sw/DataAccess/tests/test-query#graphData"))))
                          (rf (triple-object (first (get-triples mg :subject subj
                                :predicate "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#result")))))
                     (cond
                       ((not (and qf rf)) (incf skip))
                       (t (let ((r (run-one-eval-test dir qf df rf gfs)))
                            (case r
                              (:pass (incf pass))
                              (:skip (incf skip))
                              (t (incf fail) (format t "  FAIL  ~A~%" name))))))))
                  ((equal typ "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#PositiveSyntaxTest11")
                   (let ((qf (triple-object (first (get-triples mg :subject subj
                               :predicate "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#action")))))
                     (when qf
                       (handler-case
                           (sb-ext:with-timeout 5
                             (parse-sparql (slurp (merge-pathnames qf dir))) (incf pass))
                         (error () (incf fail) (format t "  FAIL  ~A~%" name))
                         (sb-ext:timeout () (incf fail) (format t "  FAIL  ~A (timeout)~%" name))))))
                  ((equal typ "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#NegativeSyntaxTest11")
                   (let ((qf (triple-object (first (get-triples mg :subject subj
                               :predicate "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#action")))))
                     (when qf
                       (handler-case
                           (sb-ext:with-timeout 5
                             (parse-sparql (slurp (merge-pathnames qf dir)))
                             (incf fail) (format t "  FAIL  ~A (should reject)~%" name))
                         (error () (incf pass))
                         (sb-ext:timeout () (incf fail) (format t "  FAIL  ~A (timeout)~%" name))))))
                  (t (incf skip))))
            (error () (incf skip))))))
    (values pass fail skip)))

;; Run all categories when loaded directly
(when (member "--run-all" sb-ext:*posix-argv* :test #'equal)
  (let ((tp 0) (tf 0) (te 0))
    (dolist (cat '("bind" "bindings" "cast" "construct" "exists"
                   "functions" "grouping" "negation" "project-expression"
                   "property-path" "subquery" "syntax-query" "aggregates"))
      (format t "~%=== ~A ===~%" cat)
      (handler-case
          (multiple-value-bind (p f e) (run-category cat)
            (format t "  ~A pass, ~A fail, ~A errors~%" p f e)
            (incf tp p) (incf tf f) (incf te e))
        (serious-condition (e) (format t "  CRASHED: ~A~%" (type-of e)))))
    (format t "~%=== TOTAL: ~A/~A pass ===~%" tp (+ tp tf te))))
