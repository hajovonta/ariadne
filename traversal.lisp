;;;; traversal.lisp
;;;; Gremlin-like graph traversal

(in-package #:ariadne)

;;; ==========================================================================
;;; Core Traversal
;;; ==========================================================================

(defun traverse (g start &rest steps)
  "Traverse the graph from START applying STEPS sequentially.
Each step is a list like (out \"knows\"), (in \"knows\"), (has \"age\" (> 30)), (values \"age\")."
  (let ((current (list start)))
    (dolist (step steps current)
      (setf current (apply-step g current step)))))

(defun apply-step (g nodes step)
  (let ((op (symbol-name (first step))))
    (cond
      ((string-equal op "OUT") (traverse-out g nodes (second step)))
      ((string-equal op "IN") (traverse-in g nodes (second step)))
      ((string-equal op "BOTH") (traverse-both g nodes (second step)))
      ((string-equal op "HAS") (apply-has-filter g nodes (second step) (third step)))
      ((string-equal op "VALUES") (apply-values g nodes (second step)))
      (t (error "Unknown traversal step: ~A" op)))))

(defun traverse-out (g nodes predicate)
  (let (results)
    (dolist (n nodes results)
      (dolist (tr (get-triples g :subject n :predicate predicate))
        (pushnew (triple-object tr) results :test #'equal)))))

(defun traverse-in (g nodes predicate)
  (let (results)
    (dolist (n nodes results)
      (dolist (tr (get-triples g :predicate predicate :object n))
        (pushnew (triple-subject tr) results :test #'equal)))))

(defun traverse-both (g nodes predicate)
  (append (traverse-out g nodes predicate)
          (traverse-in g nodes predicate)))

(defun apply-has-filter (g nodes predicate value)
  (remove-if-not
   (lambda (n)
     (let ((triples (get-triples g :subject n :predicate predicate)))
       (if (null value)
           triples
           (some (lambda (tr)
                   (let ((obj (triple-object tr)))
                     (if (listp value)
                         ;; e.g. (> 30) — apply the comparison
                         (let ((op (first value))
                               (arg (second value)))
                           (funcall (symbol-function op) obj arg))
                         (equal obj value))))
                 triples))))
   nodes))

(defun apply-values (g nodes predicate)
  (let (results)
    (dolist (n nodes results)
      (dolist (tr (get-triples g :subject n :predicate predicate))
        (push (triple-object tr) results)))))

;;; ==========================================================================
;;; Path Tracking
;;; ==========================================================================

(defun traverse-with-path (g start &rest steps)
  "Like traverse but returns full paths."
  (let ((paths (list (list start))))
    (dolist (step steps paths)
      (setf paths (extend-paths g paths step)))))

(defun extend-paths (g paths step)
  (let ((op (symbol-name (first step)))
        (pred (second step))
        (results nil))
    (dolist (path paths results)
      (let ((current (car (last path))))
        (let ((nexts (cond
                       ((string-equal op "OUT")
                        (mapcar #'triple-object
                                (get-triples g :subject current :predicate pred)))
                       ((string-equal op "IN")
                        (mapcar #'triple-subject
                                (get-triples g :predicate pred :object current)))
                       (t nil))))
          (dolist (n nexts)
            (push (append path (list n)) results)))))))

;;; ==========================================================================
;;; Depth-Limited Traversal
;;; ==========================================================================

(defun traverse-depth (g start predicate &key (max-depth 1))
  "Traverse outgoing edges up to MAX-DEPTH hops. Returns all reachable nodes."
  (let ((visited (make-hash-table :test 'equal))
        (results nil))
    (setf (gethash start visited) t)
    (labels ((walk (node depth)
               (when (< depth max-depth)
                 (dolist (tr (get-triples g :subject node :predicate predicate))
                   (let ((next (triple-object tr)))
                     (unless (gethash next visited)
                       (setf (gethash next visited) t)
                       (push next results)
                       (walk next (1+ depth))))))))
      (walk start 0))
    results))

;;; ==========================================================================
;;; Shortest Path (BFS)
;;; ==========================================================================

(defun shortest-path (g from to &key edge-type)
  "Find shortest path from FROM to TO using BFS."
  (when (equal from to) (return-from shortest-path (list from)))
  (let ((visited (make-hash-table :test 'equal))
        (queue (list (list from))))
    (setf (gethash from visited) t)
    (loop while queue do
      (let ((path (pop queue)))
        (let ((current (car (last path))))
          (dolist (tr (get-triples g :subject current :predicate edge-type))
            (let ((next (triple-object tr)))
              (cond
                ((equal next to)
                 (return-from shortest-path (append path (list next))))
                ((not (gethash next visited))
                 (setf (gethash next visited) t)
                 (setf queue (append queue (list (append path (list next))))))))))))))
