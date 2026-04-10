;;;; events.lisp
;;;; Graph events and webhooks

(in-package #:ariadne)

(defun graph-events (g)
  "Return registered event hooks for graph G."
  (getf (graph-extra g) :events))

(defun (setf graph-events) (val g)
  (setf (getf (graph-extra g) :events) val))

(defun on-graph-event (g name &key (event :all) callback url)
  "Register an event hook. Provide CALLBACK (function) and/or URL (webhook endpoint)."
  (push (list :name name :event event :callback callback :url url)
        (graph-events g)))

(defun remove-graph-event (g name)
  "Remove an event hook by name."
  (setf (graph-events g)
        (remove name (graph-events g)
                :key (lambda (h) (getf h :name)) :test #'string=)))

(defun event-to-json (evt)
  "Serialize an event plist to JSON."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "event" ht) (string-downcase (symbol-name (getf evt :event))))
    (setf (gethash "subject" ht) (princ-to-string (getf evt :subject)))
    (setf (gethash "predicate" ht) (princ-to-string (getf evt :predicate)))
    (setf (gethash "object" ht) (princ-to-string (getf evt :object)))
    (jzon:stringify ht)))

(defun fire-graph-events (g event-type subject predicate object)
  "Fire all matching event hooks."
  (dolist (hook (graph-events g))
    (let ((htype (getf hook :event)))
      (when (or (eq htype :all) (eq htype event-type))
        (let ((evt (list :event event-type
                         :subject subject :predicate predicate :object object)))
          (when (getf hook :callback)
            (funcall (getf hook :callback) evt))
          (when (getf hook :url)
            (ignore-errors
              (drakma:http-request (getf hook :url)
                                   :method :post
                                   :content-type "application/json"
                                   :content (event-to-json evt)))))))))
