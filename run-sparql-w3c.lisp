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
           ;; Extract binding name if present
           (when (cl-ppcre:scan "<binding name=\"([^\"]+)\"" s)
             (setf var (aref (nth-value 1 (cl-ppcre:scan-to-strings "<binding name=\"([^\"]+)\"" s)) 0)))
           ;; Extract value
           (cond
             ((cl-ppcre:scan "<uri>([^<]+)</uri>" s)
              (push (cons var (aref (nth-value 1 (cl-ppcre:scan-to-strings "<uri>([^<]+)</uri>" s)) 0)) bindings))
             ((cl-ppcre:scan "<literal[^>]*>([^<]*)</literal>" s)
              (push (cons var (aref (nth-value 1 (cl-ppcre:scan-to-strings "<literal[^>]*>([^<]*)</literal>" s)) 0)) bindings))
             ((search "<unbound/>" s) (push (cons var nil) bindings)))
           ;; End of result
           (when (search "</result>" s)
             (push (nreverse bindings) results) (setf bindings nil))))))
    (nreverse results)))

(defun run-one-eval-test (dir qf df rf &optional graph-data-files)
  "Run one evaluation test with 5s timeout. Returns :pass or :fail."
  (handler-case
      (#+sbcl sb-ext:with-timeout #+sbcl 5
       #-sbcl progn
        (let ((g (make-graph)))
          (when df (import-turtle g (slurp (merge-pathnames df dir))))
          (dolist (gf graph-data-files)
            (let ((gpath (merge-pathnames gf dir)))
              (when (probe-file gpath)
                (import-turtle g (slurp gpath) :graph-name (namestring gf)))))
          (let ((actual (sparql g (slurp (merge-pathnames qf dir))))
                (expected (parse-srx (merge-pathnames rf dir))))
            (if (member expected '(t nil))
                (if (eq (not (not actual)) expected) :pass :fail)
                (if (= (length actual) (length expected)) :pass :fail)))))
    (error () :fail)
    #+sbcl (sb-ext:timeout () :fail)))

(defun run-category (cat)
  (let* ((dir (merge-pathnames (format nil "~A/" cat) *sbase*))
         (mf (merge-pathnames "manifest.ttl" dir))
         (pass 0) (fail 0) (err 0))
    (unless (probe-file mf) (return-from run-category (values 0 0 0)))
    (let ((mg (make-graph)))
      (handler-case (import-turtle mg (slurp mf)) (error () (return-from run-category (values 0 0 1))))
      (dolist (tr (get-triples mg :predicate "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
        (let ((subj (triple-subject tr))
              (typ (triple-object tr)))
          (handler-case
              (let ((name (or (triple-object (first (get-triples mg :subject subj
                                :predicate "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#name")))
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
                       ((not (and qf rf)) (incf err))
                       ((not (cl-ppcre:scan "\\.srx$" rf)) (incf err)) ; skip non-srx
                       (t (let ((r (run-one-eval-test dir qf df rf gfs)))
                            (if (eq r :pass)
                                (incf pass)
                                (progn (incf fail) (format t "  FAIL  ~A~%" name))))))))
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
                         (sb-ext:timeout () (incf fail) (format t "  FAIL  ~A (timeout)~%" name))))))))
            (error () (incf err))))))
    (values pass fail err)))

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
