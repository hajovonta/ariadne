;;;; benchmarks/run.lisp
;;;; Simple benchmarks for Ariadne

(in-package #:ariadne)

(defun benchmark-import (file)
  "Benchmark importing a Turtle file. Returns (triples seconds)."
  (let ((data (uiop:read-file-string file))
        (g (make-graph)))
    (let ((start (get-internal-real-time)))
      (import-turtle g data)
      (let ((elapsed (/ (- (get-internal-real-time) start)
                        internal-time-units-per-second)))
        (format t "~A: ~A triples in ~,3F seconds (~,0F triples/sec)~%"
                (file-namestring file)
                (triple-count g)
                elapsed
                (/ (triple-count g) (max elapsed 0.001)))
        (values g (triple-count g) elapsed)))))

(defun benchmark-import-nt (file)
  "Benchmark importing an N-Triples file."
  (let ((data (uiop:read-file-string file))
        (g (make-graph)))
    (let ((start (get-internal-real-time)))
      (import-ntriples g data)
      (let ((elapsed (/ (- (get-internal-real-time) start)
                        internal-time-units-per-second)))
        (format t "~A: ~A triples in ~,3F seconds (~,0F triples/sec)~%"
                (file-namestring file)
                (triple-count g)
                elapsed
                (/ (triple-count g) (max elapsed 0.001)))
        (values g (triple-count g) elapsed)))))

(defun benchmark-queries (g)
  "Run some standard queries and time them."
  (flet ((time-query (name expr)
           (let ((start (get-internal-real-time)))
             (let ((results (query g expr)))
               (let ((elapsed (/ (- (get-internal-real-time) start)
                                 internal-time-units-per-second)))
                 (format t "  ~A: ~A results in ~,3F seconds~%"
                         name (length results) elapsed))))))
    (format t "Queries on ~A triples:~%" (triple-count g))
    (time-query "all subjects" '(select-distinct (?s) (where (?s ?p ?o))))
    (time-query "all types" '(select-distinct (?type)
                              (where (?x "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" ?type))))
    (time-query "count triples" '(select ((count ?s)) (where (?s ?p ?o))))))

(defun run-benchmarks ()
  (let ((bench-dir (merge-pathnames "benchmarks/"
                                     (asdf:system-source-directory :ariadne))))
    ;; Turtle files
    (dolist (file (directory (merge-pathnames "*.ttl" bench-dir)))
      (format t "~%--- ~A ---~%" (file-namestring file))
      (handler-case
          (multiple-value-bind (g count elapsed) (benchmark-import file)
            (when (> count 0)
              (benchmark-queries g)))
        (error (e)
          (format t "  ERROR: ~A~%" e))))
    ;; N-Triples files
    (dolist (file (directory (merge-pathnames "*.nt" bench-dir)))
      (format t "~%--- ~A ---~%" (file-namestring file))
      (handler-case
          (multiple-value-bind (g count elapsed) (benchmark-import-nt file)
            (when (> count 0)
              (benchmark-queries g)))
        (error (e)
          (format t "  ERROR: ~A~%" e))))))
