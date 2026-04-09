;;;; tests/suite-w3c-turtle.lisp
;;;; W3C Turtle conformance tests

(in-package #:ariadne/tests)
(in-suite :w3c-turtle)

(defun w3c-test-dir ()
  (merge-pathnames "test-data/w3c-turtle/TurtleTests/"
                   (asdf:system-source-directory :ariadne-tests)))

(defun w3c-positive-tests ()
  "Return list of TTL files that should parse successfully."
  (let ((dir (w3c-test-dir)))
    (when (probe-file dir)
      (remove-if (lambda (p)
                   (let ((name (file-namestring p)))
                     (or (search "syntax-bad" name)
                         (search "eval-bad" name)
                         (string= "manifest.ttl" name))))
                 (directory (merge-pathnames "*.ttl" dir))))))

(defun w3c-negative-tests ()
  "Return list of TTL files that should fail to parse (syntax-bad + eval-bad)."
  (let ((dir (w3c-test-dir)))
    (when (probe-file dir)
      (remove-if-not (lambda (p)
                       (let ((name (file-namestring p)))
                         (or (search "syntax-bad" name)
                             (search "eval-bad" name))))
                     (directory (merge-pathnames "*.ttl" dir))))))

(test w3c-positive-parse
  "All W3C positive Turtle tests should parse without error"
  (let ((files (w3c-positive-tests))
        (passed 0)
        (failed nil))
    (dolist (file files)
      (handler-case
          (let ((g (make-graph)))
            (import-turtle g (uiop:read-file-string file))
            (incf passed))
        (error (e)
          (push (cons (file-namestring file) (princ-to-string e)) failed))))
    (format t "~%W3C Positive: ~A/~A passed~%" passed (length files))
    (when failed
      (format t "Failed files:~%")
      (dolist (f (subseq failed 0 (min 20 (length failed))))
        (format t "  ~A: ~A~%" (car f) (subseq (cdr f) 0 (min 80 (length (cdr f)))))))
    (is (= 0 (length failed)))))

(test w3c-negative-parse
  "All W3C negative Turtle tests should signal an error"
  (let ((files (w3c-negative-tests))
        (correctly-rejected 0)
        (incorrectly-accepted nil))
    (dolist (file files)
      (handler-case
          (let ((g (make-graph)))
            (import-turtle g (uiop:read-file-string file))
            (push (file-namestring file) incorrectly-accepted))
        (error ()
          (incf correctly-rejected))))
    (format t "~%W3C Negative: ~A/~A correctly rejected~%"
            correctly-rejected (length files))
    (when incorrectly-accepted
      (format t "Incorrectly accepted:~%")
      (dolist (f incorrectly-accepted)
        (format t "  ~A~%" f)))
    (is (= 0 (length incorrectly-accepted)))))

;;; ==========================================================================
;;; Per-category negative tests for targeted development
;;; ==========================================================================

(defun negative-files-matching (substring)
  "Return negative test files whose name contains SUBSTRING."
  (remove-if-not (lambda (p) (search substring (file-namestring p)))
                 (w3c-negative-tests)))

(defun check-all-rejected (files)
  "Return list of files that were incorrectly accepted."
  (let (accepted)
    (dolist (file files accepted)
      (handler-case
          (let ((g (make-graph)))
            (import-turtle g (uiop:read-file-string file))
            (push (file-namestring file) accepted))
        (error () nil)))))

(test w3c-neg-uri
  "Reject invalid URIs"
  (let ((bad (check-all-rejected (negative-files-matching "bad-uri"))))
    (is (null bad) "Accepted: ~A" bad)))

(test w3c-neg-struct
  "Reject structural errors"
  (let ((bad (check-all-rejected (negative-files-matching "bad-struct"))))
    (is (null bad) "Accepted: ~A" bad)))

(test w3c-neg-string
  "Reject bad string literals"
  (let ((bad (check-all-rejected (negative-files-matching "bad-string"))))
    (is (null bad) "Accepted: ~A" bad)))

(test w3c-neg-escape
  "Reject bad escape sequences"
  (let ((bad (check-all-rejected (negative-files-matching "bad-esc"))))
    (is (null bad) "Accepted: ~A" bad)))

(test w3c-neg-prefix
  "Reject bad prefix declarations and undefined prefixes"
  (let ((bad (check-all-rejected (negative-files-matching "bad-prefix"))))
    (is (null bad) "Accepted: ~A" bad)))

(test w3c-neg-number
  "Reject malformed numeric literals"
  (let ((bad (check-all-rejected
              (append (negative-files-matching "bad-num")
                      (negative-files-matching "bad-number")))))
    (is (null bad) "Accepted: ~A" bad)))

(test w3c-neg-keyword
  "Reject invalid keywords"
  (let ((bad (check-all-rejected (negative-files-matching "bad-kw"))))
    (is (null bad) "Accepted: ~A" bad)))

(test w3c-neg-n3-extras
  "Reject N3 syntax not valid in Turtle"
  (let ((bad (check-all-rejected (negative-files-matching "bad-n3"))))
    (is (null bad) "Accepted: ~A" bad)))

(test w3c-neg-base
  "Reject malformed @base declarations"
  (let ((bad (check-all-rejected (negative-files-matching "bad-base"))))
    (is (null bad) "Accepted: ~A" bad)))

(test w3c-neg-lang
  "Reject bad language tags"
  (let ((bad (check-all-rejected (negative-files-matching "bad-lang"))))
    (is (null bad) "Accepted: ~A" bad)))

(test w3c-neg-pname
  "Reject bad prefixed names"
  (let ((bad (check-all-rejected (negative-files-matching "bad-pname"))))
    (is (null bad) "Accepted: ~A" bad)))

(test w3c-neg-ns-dot
  "Reject dots in namespace prefixes"
  (let ((bad (check-all-rejected
              (append (negative-files-matching "bad-ns-dot")
                      (negative-files-matching "bad-missing-ns")))))
    (is (null bad) "Accepted: ~A" bad)))

(test w3c-neg-ln
  "Reject bad local names"
  (let ((bad (check-all-rejected
              (append (negative-files-matching "bad-ln")
                      (negative-files-matching "bad-blank-label")))))
    (is (null bad) "Accepted: ~A" bad)))

(test w3c-neg-literal-langtag-datatype
  "Reject literal with both language tag and datatype"
  (let ((bad (check-all-rejected (negative-files-matching "LITERAL2"))))
    (is (null bad) "Accepted: ~A" bad)))

(test w3c-neg-eval
  "Reject evaluation errors (undefined prefixes at eval time)"
  (let ((bad (check-all-rejected (negative-files-matching "eval-bad"))))
    (is (null bad) "Accepted: ~A" bad)))
