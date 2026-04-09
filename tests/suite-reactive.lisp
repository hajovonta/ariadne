;;;; tests/suite-reactive.lisp
;;;; Reactive queries: triggers that fire when patterns appear/disappear

(in-package #:ariadne/tests)
(in-suite :reactive)

;; =============================================================================
;; On-Match Triggers
;; =============================================================================

(test on-match-fires-on-add
  "Trigger fires when a matching triple is added"
  (let ((g (make-graph))
        (fired nil))
    (on-match g :alert-trigger
      :pattern '(?x "type" "alert")
      :callback (lambda (triple) (push triple fired)))
    (add-triple g "event1" "type" "alert")
    (is (= 1 (length fired)))))

(test on-match-no-fire-on-mismatch
  "Trigger does not fire for non-matching triples"
  (let ((g (make-graph))
        (fired nil))
    (on-match g :alert-trigger
      :pattern '(?x "type" "alert")
      :callback (lambda (triple) (push triple fired)))
    (add-triple g "event1" "type" "info")
    (is (= 0 (length fired)))))

(test on-match-fires-multiple
  "Trigger fires for each matching triple"
  (let ((g (make-graph))
        (fired nil))
    (on-match g :alert-trigger
      :pattern '(?x "type" "alert")
      :callback (lambda (triple) (push triple fired)))
    (add-triple g "event1" "type" "alert")
    (add-triple g "event2" "type" "alert")
    (add-triple g "event3" "type" "info")
    (is (= 2 (length fired)))))

(test on-match-specific-subject
  "Trigger with bound subject"
  (let ((g (make-graph))
        (fired nil))
    (on-match g :alice-trigger
      :pattern '("alice" "status" ?x)
      :callback (lambda (triple) (push triple fired)))
    (add-triple g "alice" "status" "online")
    (add-triple g "bob" "status" "online")
    (is (= 1 (length fired)))))

;; =============================================================================
;; Trigger Management
;; =============================================================================

(test remove-trigger
  "Remove a trigger by name"
  (let ((g (make-graph))
        (fired nil))
    (on-match g :my-trigger
      :pattern '(?x "type" "alert")
      :callback (lambda (triple) (push triple fired)))
    (remove-trigger g :my-trigger)
    (add-triple g "event1" "type" "alert")
    (is (= 0 (length fired)))))

(test list-triggers
  "List all active triggers"
  (let ((g (make-graph)))
    (on-match g :trigger-1
      :pattern '(?x "type" "a")
      :callback (lambda (tr) (declare (ignore tr))))
    (on-match g :trigger-2
      :pattern '(?x "type" "b")
      :callback (lambda (tr) (declare (ignore tr))))
    (is (= 2 (length (graph-triggers g))))))
