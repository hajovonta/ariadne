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
  "Uncommitted changes are not visible outside the transaction"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (let ((tx (begin-transaction g)))
      (declare (ignore tx))
      (add-triple g "alice" "knows" "charlie")
      ;; Outside the transaction, charlie should not be visible yet
      ;; (This tests snapshot isolation)
      (is (= 1 (triple-count g :snapshot :before-transaction))))))

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
    (is (= 1 (triple-count g)))))
