;;;; profiling.lisp
;;;; Query profiling and graph statistics

(in-package #:ariadne)

(defun profile-query (g expr)
  "Execute a query and return a profiling report string."
  (let* ((start (get-internal-real-time))
         (results (query g expr))
         (elapsed (/ (- (get-internal-real-time) start)
                     (float internal-time-units-per-second)))
         (count (if (listp results) (length results) (if results 1 0))))
    (format nil "Query time: ~,4F seconds, ~A results" elapsed count)))

(defun graph-statistics (g)
  "Return a plist of graph statistics."
  (list :name (graph-name g)
        :triples (triple-count g)
        :subjects (length (all-subjects g))
        :predicates (length (all-predicates g))
        :objects (length (all-objects g))
        :spo-index-size (hash-table-count (graph-spo g))
        :interned-strings (hash-table-count *intern-table*)))
