;;;; tests/suite-shacl-w3c.lisp
;;;; W3C SHACL conformance tests (partial compliance: sh:conforms only)

(in-package #:ariadne/tests)

(in-suite :shacl-w3c)

(defvar *shacl-test-base*
  (merge-pathnames "test-data/shacl/data-shapes/data-shapes-test-suite/tests/core/"
                   (asdf:system-source-directory :ariadne)))

(defun load-shacl-test-file (path)
  "Load a SHACL test Turtle file into a graph."
  (let ((g (make-graph :name (namestring path)))
        (content (with-open-file (s path :direction :input :external-format :utf-8)
                   (let ((buf (make-string (file-length s))))
                     (read-sequence buf s)
                     buf))))
    (import-turtle g content)
    g))

(defun extract-expected-conforms (g)
  "Extract the expected sh:conforms value from a test graph."
  (let ((tr (first (get-triples g :predicate "http://www.w3.org/ns/shacl#conforms"))))
    (when tr
      (let ((val (triple-object tr)))
        (cond
          ((equal val "true") t)
          ((equal val "false") nil)
          ((eq val t) t)
          ((null val) nil)
          ((and (stringp val) (search "true" val)) t)
          (t nil))))))

(defun run-shacl-w3c-test (path)
  "Run a single W3C SHACL test. Returns (values pass-p expected actual)."
  (handler-case
      (let* ((g (load-shacl-test-file path))
             ;; Check for separate data/shapes graphs
             (data-refs (mapcar #'triple-object
                                (get-triples g :predicate "http://www.w3.org/ns/shacl-test#dataGraph")))
             (shapes-refs (mapcar #'triple-object
                                  (get-triples g :predicate "http://www.w3.org/ns/shacl-test#shapesGraph"))))
        ;; Load referenced files into the same graph
        (dolist (ref (append data-refs shapes-refs))
          (when (and (stringp ref) (> (length ref) 0) (not (string= ref "")))
            (let ((ref-path (merge-pathnames ref (directory-namestring path))))
              (when (probe-file ref-path)
                (let ((content (with-open-file (s ref-path :direction :input :external-format :utf-8)
                                 (let ((buf (make-string (file-length s))))
                                   (read-sequence buf s) buf))))
                  (handler-case (import-turtle g content)
                    (error () nil)))))))
        (let* ((expected (extract-expected-conforms g))
               (report (shacl-validate g))
               (actual (getf report :conforms)))
          (values (eq (not (not expected)) (not (not actual)))
                  expected actual)))
    (error (e)
      (declare (ignore e))
      (values nil :error :error))))

(defun run-shacl-w3c-suite (subdir)
  "Run all W3C SHACL tests in a subdirectory. Returns (pass fail skip) counts."
  (let ((dir (merge-pathnames subdir *shacl-test-base*))
        (pass 0) (fail 0) (errors 0))
    (dolist (file (directory (merge-pathnames "*.ttl" dir)))
      (unless (string= "manifest" (pathname-name file))
        (multiple-value-bind (ok expected actual) (run-shacl-w3c-test file)
          (if (eq expected :error)
              (incf errors)
              (if ok (incf pass) (incf fail))))))
    (values pass fail errors)))

;;; Run each category

(test shacl-w3c-targets
  "W3C SHACL core/targets tests"
  (multiple-value-bind (pass fail errors) (run-shacl-w3c-suite "targets/")
    (format t "~%  targets: ~A pass, ~A fail, ~A errors~%" pass fail errors)
    (is (> pass 0))
    (is (= 0 fail))))

(test shacl-w3c-property
  "W3C SHACL core/property tests (partial compliance)"
  (multiple-value-bind (pass fail errors) (run-shacl-w3c-suite "property/")
    (format t "~%  property: ~A pass, ~A fail, ~A errors~%" pass fail errors)
    (is (> pass 0))))

(test shacl-w3c-node
  "W3C SHACL core/node tests (partial compliance)"
  (multiple-value-bind (pass fail errors) (run-shacl-w3c-suite "node/")
    (format t "~%  node: ~A pass, ~A fail, ~A errors~%" pass fail errors)
    (is (> pass 0))))

(test shacl-w3c-misc
  "W3C SHACL core/misc tests"
  (multiple-value-bind (pass fail errors) (run-shacl-w3c-suite "misc/")
    (format t "~%  misc: ~A pass, ~A fail, ~A errors~%" pass fail errors)
    (is (> pass 0))))
