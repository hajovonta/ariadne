;;;; inference.lisp
;;;; Forward-chaining inference rules

(in-package #:ariadne)

;;; ==========================================================================
;;; Rule Storage
;;; ==========================================================================

;;; Rules are stored on the graph as a plist property.
;;; Each rule: (:name name :when patterns :then templates)

(defun graph-rules (g)
  (getf (graph-extra g) :rules))

(defun (setf graph-rules) (val g)
  (setf (getf (graph-extra g) :rules) val))

(defun defrule (g name &key when then)
  "Define an inference rule on graph G."
  (let ((rule (list :name name :when when :then then)))
    (push rule (graph-rules g))
    rule))

(defun remove-rule (g name)
  "Remove a rule by name."
  (setf (graph-rules g)
        (remove name (graph-rules g) :key (lambda (r) (getf r :name)))))

;;; ==========================================================================
;;; Rule Application (Forward Chaining)
;;; ==========================================================================

(defun apply-rules (g)
  "Apply all rules until no new triples are generated (fixed point)."
  (loop
    (let ((new-count 0))
      (dolist (rule (graph-rules g))
        (incf new-count (apply-single-rule g rule)))
      (when (= 0 new-count)
        (return)))))

(defun apply-single-rule (g rule)
  "Apply a single rule, returning the number of new triples added."
  (let ((when-patterns (getf rule :when))
        (then-templates (getf rule :then))
        (count 0))
    (let ((envs (match-patterns g when-patterns)))
      (dolist (env envs)
        (dolist (template then-templates)
          (let ((s (resolve-template (first template) env))
                (p (resolve-template (second template) env))
                (o (resolve-template (third template) env)))
            (unless (has-triple-p g s p o)
              (add-triple g s p o)
              (incf count))))))
    count))

(defun resolve-template (x env)
  "Resolve a template element: substitute variables from env."
  (if (variable-p x)
      (or (lookup-binding x env) x)
      x))
