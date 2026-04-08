;;;; graph-export.lisp
;;;; Export graphs to DOT/Graphviz format

(in-package #:ariadne)

(defun export-dot (g &key predicates center depth file)
  "Export graph as DOT format string.
OPTIONS:
  :predicates — list of predicates to include (nil = all)
  :center — node ID to center subgraph on
  :depth — max hops from center (requires :center)
  :file — write to file instead of returning string"
  (let ((triples (select-triples-for-dot g predicates center depth)))
    (let ((dot (generate-dot (or (graph-name g) "ariadne") triples)))
      (if file
          (progn
            (with-open-file (s file :direction :output :if-exists :supersede)
              (write-string dot s))
            dot)
          dot))))

(defun select-triples-for-dot (g predicates center depth)
  "Select triples for DOT export based on filters."
  (let ((triples (if predicates
                     (loop for p in predicates
                           append (get-triples g :predicate p))
                     (get-triples g))))
    (if (and center depth)
        (let ((reachable (collect-neighborhood g center depth)))
          (remove-if-not
           (lambda (tr)
             (and (gethash (triple-subject tr) reachable)
                  (gethash (triple-object tr) reachable)))
           triples))
        triples)))

(defun collect-neighborhood (g center depth)
  "Collect all nodes within DEPTH hops of CENTER (both directions)."
  (let ((visited (make-hash-table :test 'equal)))
    (setf (gethash center visited) t)
    (let ((frontier (list center)))
      (dotimes (i depth)
        (let ((next nil))
          (dolist (node frontier)
            (dolist (tr (get-triples g :subject node))
              (let ((o (triple-object tr)))
                (when (and (stringp o) (not (gethash o visited)))
                  (setf (gethash o visited) t)
                  (push o next))))
            (dolist (tr (get-triples g :object node))
              (let ((s (triple-subject tr)))
                (unless (gethash s visited)
                  (setf (gethash s visited) t)
                  (push s next)))))
          (setf frontier next))))
    visited))

(defun generate-dot (name triples)
  "Generate DOT format string from triples."
  (with-output-to-string (s)
    (format s "digraph ~A {~%" (dot-escape-id name))
    (let ((nodes (make-hash-table :test 'equal)))
      ;; Collect nodes and emit edges
      (dolist (tr triples)
        (let ((subj (princ-to-string (triple-subject tr)))
              (pred (princ-to-string (triple-predicate tr)))
              (obj (princ-to-string (triple-object tr))))
          (setf (gethash subj nodes) t)
          (setf (gethash obj nodes) t)
          (format s "  ~A -> ~A [label=~A];~%"
                  (dot-quote subj) (dot-quote obj) (dot-quote pred))))
      ;; Emit node declarations
      (maphash (lambda (node _)
                 (declare (ignore _))
                 (format s "  ~A;~%" (dot-quote node)))
               nodes))
    (format s "}~%")))

(defun dot-escape-id (name)
  "Escape a graph name for DOT."
  (substitute #\_ #\Space (substitute #\_ #\- name)))

(defun dot-quote (str)
  "Quote a string for DOT."
  (format nil "\"~A\"" (remove #\" str)))
