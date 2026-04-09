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

(defun hash-table-byte-estimate (ht)
  "Rough estimate of hash table memory in bytes."
  ;; ~64 bytes overhead + ~32 bytes per bucket
  (+ 64 (* 32 (hash-table-size ht))))

(defun graph-memory-usage (g)
  "Return a plist estimating memory usage of graph G."
  (let ((index-bytes (+ (hash-table-byte-estimate (graph-spo g))
                        (hash-table-byte-estimate (graph-sp g))
                        (hash-table-byte-estimate (graph-s g))
                        (hash-table-byte-estimate (graph-p g))
                        (hash-table-byte-estimate (graph-po g))
                        (hash-table-byte-estimate (graph-o g))
                        (hash-table-byte-estimate (graph-os g))
                        (hash-table-byte-estimate (graph-triple-graph g))
                        (hash-table-byte-estimate (graph-graph-index g)))))
    (list :triple-count (triple-count g)
          :index-bytes index-bytes
          :intern-count (hash-table-count *intern-table*))))
