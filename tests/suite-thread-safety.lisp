;;;; tests/suite-thread-safety.lisp
;;;; Thread safety: concurrent reads and writes

(in-package #:ariadne/tests)
(in-suite :thread-safety)

;; =============================================================================
;; Concurrent Reads
;; =============================================================================

(test concurrent-reads
  "Multiple threads can read the graph concurrently"
  (let ((g (make-graph)))
    (dotimes (i 1000)
      (add-triple g (format nil "node-~A" i) "type" "node"))
    (let ((results (make-array 4 :initial-element nil)))
      (let ((threads
              (loop for idx below 4
                    collect (let ((i idx))
                              (bt:make-thread
                               (lambda ()
                                 (setf (aref results i)
                                       (length (get-triples g :predicate "type")))))))))
        (mapc #'bt:join-thread threads))
      (is (every (lambda (r) (= 1000 r)) (coerce results 'list))))))

;; =============================================================================
;; Concurrent Writes
;; =============================================================================

(test concurrent-writes
  "Multiple threads can write to the graph"
  (let ((g (make-graph)))
    (let ((threads
            (loop for idx below 4
                  collect (let ((i idx))
                            (bt:make-thread
                             (lambda ()
                               (dotimes (j 250)
                                 (add-triple g
                                             (format nil "t~A-node-~A" i j)
                                             "type" "node"))))))))
      (mapc #'bt:join-thread threads))
    ;; All 1000 triples should be present
    (is (= 1000 (triple-count g)))))

;; =============================================================================
;; Read-Write Concurrent
;; =============================================================================

(test concurrent-read-write
  "Reads and writes can happen concurrently without crashing"
  (let ((g (make-graph))
        (read-count 0))
    ;; Pre-populate
    (dotimes (i 100)
      (add-triple g (format nil "init-~A" i) "type" "node"))
    (let ((writer (bt:make-thread
                   (lambda ()
                     (dotimes (i 500)
                       (add-triple g (format nil "new-~A" i) "type" "node")))))
          (reader (bt:make-thread
                   (lambda ()
                     (dotimes (i 500)
                       (let ((r (get-triples g :predicate "type")))
                         (setf read-count (length r))))))))
      (bt:join-thread writer)
      (bt:join-thread reader))
    ;; Writer should have added all triples
    (is (= 600 (triple-count g)))
    ;; Reader should have gotten some results
    (is (> read-count 0))))
