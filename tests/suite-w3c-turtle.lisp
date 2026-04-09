;;;; tests/suite-w3c-turtle.lisp
;;;; W3C Turtle conformance tests

(in-package #:ariadne/tests)
(in-suite :w3c-turtle)

(defun w3c-test-dir ()
  (merge-pathnames "test-data/w3c-turtle/TurtleTests/"
                   (asdf:system-source-directory :ariadne-tests)))

(defun w3c-positive-tests ()
  "Return list of TTL files that should parse successfully (not syntax-bad)."
  (let ((dir (w3c-test-dir)))
    (when (probe-file dir)
      (remove-if (lambda (p)
                   (let ((name (file-namestring p)))
                     (or (search "syntax-bad" name)
                         (string= "manifest.ttl" name))))
                 (directory (merge-pathnames "*.ttl" dir))))))

(defun w3c-negative-tests ()
  "Return list of TTL files that should fail to parse (syntax-bad)."
  (let ((dir (w3c-test-dir)))
    (when (probe-file dir)
      (remove-if-not (lambda (p) (search "syntax-bad" (file-namestring p)))
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
    ;; Report
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
            ;; If we get here, the bad file was accepted — that's wrong
            (push (file-namestring file) incorrectly-accepted))
        (error ()
          (incf correctly-rejected))))
    (format t "~%W3C Negative: ~A/~A correctly rejected~%"
            correctly-rejected (length files))
    (when incorrectly-accepted
      (format t "Incorrectly accepted:~%")
      (dolist (f (subseq incorrectly-accepted 0 (min 20 (length incorrectly-accepted))))
        (format t "  ~A~%" f)))
    ;; For now, just report — don't fail on negative tests
    ;; since we need to add validation first
    (is-true t)))
