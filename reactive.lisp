;;;; reactive.lisp
;;;; Reactive queries: triggers that fire when patterns match

(in-package #:ariadne)

;;; Triggers are stored on the graph's extra plist.
;;; Each trigger: (:name name :pattern (s p o) :callback fn)

(defun graph-triggers (g)
  (getf (graph-extra g) :triggers))

(defun (setf graph-triggers) (val g)
  (setf (getf (graph-extra g) :triggers) val))

(defun on-match (g name &key pattern callback)
  "Register a trigger that fires CALLBACK when a triple matching PATTERN is added."
  (push (list :name name :pattern pattern :callback callback)
        (graph-triggers g)))

(defun remove-trigger (g name)
  "Remove a trigger by name."
  (setf (graph-triggers g)
        (remove name (graph-triggers g) :key (lambda (tr) (getf tr :name)))))

;;; Hook into add-triple — we need to wrap it to check triggers.
;;; We do this by saving the original and redefining.

(defun check-triggers (g triple)
  "Check all triggers against a newly added triple."
  (dolist (trigger (graph-triggers g))
    (let ((pattern (getf trigger :pattern))
          (callback (getf trigger :callback)))
      (when (triple-matches-pattern-p triple pattern)
        (funcall callback triple)))))

(defun triple-matches-pattern-p (triple pattern)
  "Check if a triple matches a trigger pattern (with ?variables as wildcards)."
  (destructuring-bind (ps pp po) pattern
    (and (or (variable-p ps) (equal ps (triple-subject triple)))
         (or (variable-p pp) (equal pp (triple-predicate triple)))
         (or (variable-p po) (equal po (triple-object triple))))))
