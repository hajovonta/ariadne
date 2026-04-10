;;;; tests/suite-graph-events.lisp
;;;; Tests for graph events and webhooks

(in-package #:ariadne/tests)

(in-suite :graph-events)

(test register-webhook
  "on-graph-event registers a webhook"
  (let ((g (make-graph :name "evt-test")))
    (on-graph-event g "hook1" :event :add :callback (lambda (evt) (declare (ignore evt))))
    (is (= 1 (length (graph-events g))))))

(test event-fires-on-add
  "Webhook fires when a triple is added"
  (let ((g (make-graph :name "evt-add"))
        (fired nil))
    (on-graph-event g "hook1" :event :add
                    :callback (lambda (evt) (push evt fired)))
    (add-triple g "http://ex.org/a" "http://ex.org/b" "http://ex.org/c")
    (is (= 1 (length fired)))
    (is (eq :add (getf (first fired) :event)))))

(test event-fires-on-remove
  "Webhook fires when a triple is removed"
  (let ((g (make-graph :name "evt-rm"))
        (fired nil))
    (add-triple g "http://ex.org/a" "http://ex.org/b" "http://ex.org/c")
    (on-graph-event g "hook1" :event :remove
                    :callback (lambda (evt) (push evt fired)))
    (remove-triple g "http://ex.org/a" "http://ex.org/b" "http://ex.org/c")
    (is (= 1 (length fired)))
    (is (eq :remove (getf (first fired) :event)))))

(test event-contains-triple-data
  "Event plist contains subject, predicate, object"
  (let ((g (make-graph :name "evt-data"))
        (fired nil))
    (on-graph-event g "hook1" :event :add
                    :callback (lambda (evt) (push evt fired)))
    (add-triple g "http://ex.org/s" "http://ex.org/p" "http://ex.org/o")
    (let ((evt (first fired)))
      (is (string= "http://ex.org/s" (getf evt :subject)))
      (is (string= "http://ex.org/p" (getf evt :predicate)))
      (is (string= "http://ex.org/o" (getf evt :object))))))

(test event-all-catches-both
  "Event type :all fires on both add and remove"
  (let ((g (make-graph :name "evt-all"))
        (fired nil))
    (on-graph-event g "hook1" :event :all
                    :callback (lambda (evt) (push evt fired)))
    (add-triple g "http://ex.org/a" "http://ex.org/b" "http://ex.org/c")
    (remove-triple g "http://ex.org/a" "http://ex.org/b" "http://ex.org/c")
    (is (= 2 (length fired)))))

(test remove-graph-event
  "remove-graph-event unregisters a webhook"
  (let ((g (make-graph :name "evt-unreg"))
        (fired nil))
    (on-graph-event g "hook1" :event :add
                    :callback (lambda (evt) (push evt fired)))
    (remove-graph-event g "hook1")
    (add-triple g "http://ex.org/a" "http://ex.org/b" "http://ex.org/c")
    (is (null fired))))

(test event-to-json
  "event-to-json serializes event as JSON string"
  (let ((evt (list :event :add
                   :subject "http://ex.org/s"
                   :predicate "http://ex.org/p"
                   :object "http://ex.org/o")))
    (let ((json (event-to-json evt)))
      (is (stringp json))
      (is (search "\"event\":\"add\"" json))
      (is (search "\"subject\":\"http://ex.org/s\"" json)))))

(test webhook-url-registration
  "on-graph-event with :url stores the URL"
  (let ((g (make-graph :name "evt-url")))
    (on-graph-event g "hook1" :event :add :url "http://localhost:9999/hook")
    (let ((hook (first (graph-events g))))
      (is (string= "http://localhost:9999/hook" (getf hook :url))))))
