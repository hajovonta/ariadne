;;;; tests/suite-incremental-persistence.lisp
;;;; Append-only transaction log

(in-package #:ariadne/tests)
(in-suite :incremental-persistence)

(defparameter *test-log-path*
  (merge-pathnames "test-data/test-txlog.log"
                   (asdf:system-source-directory :ariadne-tests)))

(test txlog-write-and-replay
  "Write transaction log then replay into new graph"
  (let ((g (make-graph)))
    (unwind-protect
         (progn
           (start-txlog g *test-log-path*)
           (add-triple g "alice" "knows" "bob")
           (add-triple g "bob" "knows" "charlie")
           (remove-triple g "bob" "knows" "charlie")
           (stop-txlog g)
           (let ((g2 (make-graph)))
             (replay-txlog g2 *test-log-path*)
             (is (= 1 (triple-count g2)))
             (is-true (has-triple-p g2 "alice" "knows" "bob"))))
      (when (probe-file *test-log-path*)
        (delete-file *test-log-path*)))))

(test txlog-append-across-sessions
  "Log appends across start/stop cycles"
  (let ((g (make-graph)))
    (unwind-protect
         (progn
           (start-txlog g *test-log-path*)
           (add-triple g "alice" "knows" "bob")
           (stop-txlog g)
           (start-txlog g *test-log-path*)
           (add-triple g "bob" "knows" "charlie")
           (stop-txlog g)
           (let ((g2 (make-graph)))
             (replay-txlog g2 *test-log-path*)
             (is (= 2 (triple-count g2)))))
      (when (probe-file *test-log-path*)
        (delete-file *test-log-path*)))))

(test txlog-snapshot-and-log
  "Snapshot + txlog for full recovery"
  (let ((g (make-graph :name "test"))
        (snap-path (merge-pathnames "test-data/test-snap.ariadne"
                                    (asdf:system-source-directory :ariadne-tests))))
    (unwind-protect
         (progn
           (add-triple g "alice" "knows" "bob")
           (save-graph g snap-path)
           (start-txlog g *test-log-path*)
           (add-triple g "bob" "knows" "charlie")
           (stop-txlog g)
           ;; Recover: load snapshot then replay log
           (let ((g2 (load-graph snap-path)))
             (replay-txlog g2 *test-log-path*)
             (is (= 2 (triple-count g2)))))
      (when (probe-file *test-log-path*) (delete-file *test-log-path*))
      (when (probe-file snap-path) (delete-file snap-path)))))
