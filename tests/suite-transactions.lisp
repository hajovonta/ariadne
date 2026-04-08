;;;; tests/suite-transactions.lisp
;;;; Transaction support: begin, commit, rollback, isolation

(in-package #:ariadne/tests)
(in-suite :transactions)

;; =============================================================================
;; Basic Transactions
;; =============================================================================

(test transaction-commit
  "Changes are visible after commit"
  (let ((g (make-graph)))
    (with-transaction (g)
      (add-triple g "alice" "knows" "bob"))
    (is (= 1 (triple-count g)))))

(test transaction-rollback
  "Changes are discarded on rollback"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (handler-case
        (with-transaction (g)
          (add-triple g "alice" "knows" "charlie")
          (error "force rollback"))
      (error () nil))
    (is (= 1 (triple-count g)))
    (is-false (has-triple-p g "alice" "knows" "charlie"))))

(test transaction-explicit-rollback
  "Explicit rollback discards changes"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (let ((tx (begin-transaction g)))
      (add-triple g "alice" "knows" "charlie")
      (rollback-transaction tx))
    (is (= 1 (triple-count g)))))

;; =============================================================================
;; Isolation
;; =============================================================================

(test transaction-isolation-read
  "Transaction snapshot captures state at begin time"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (let ((tx (begin-transaction g)))
      (add-triple g "alice" "knows" "charlie")
      ;; Snapshot should have only the original triple
      (is (= 1 (length (transaction-snapshot tx)))))))

;; =============================================================================
;; transaction-snapshot direct usage
;; =============================================================================

(test transaction-snapshot-contents
  "transaction-snapshot returns the actual triple data"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (let* ((tx (begin-transaction g))
           (snap (transaction-snapshot tx)))
      (is (= 1 (length snap)))
      (is (equal '("alice" "knows" "bob") (first snap))))))

;; =============================================================================
;; Nested Transactions
;; =============================================================================

(test nested-transaction-commit
  "Nested transaction: inner commit, outer commit"
  (let ((g (make-graph)))
    (with-transaction (g)
      (add-triple g "alice" "knows" "bob")
      (with-transaction (g)
        (add-triple g "bob" "knows" "charlie")))
    (is (= 2 (triple-count g)))))

(test nested-transaction-inner-rollback
  "Nested transaction: inner rollback, outer commit"
  (let ((g (make-graph)))
    (with-transaction (g)
      (add-triple g "alice" "knows" "bob")
      (handler-case
          (with-transaction (g)
            (add-triple g "bob" "knows" "charlie")
            (error "rollback inner"))
        (error () nil)))
    ;; Only the outer triple should survive
    (is (= 1 (triple-count g)))
    (is-true (has-triple-p g "alice" "knows" "bob"))
    (is-false (has-triple-p g "bob" "knows" "charlie"))))
