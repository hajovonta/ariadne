;;;; partitioned.lisp
;;;; Graph partitioning — split across multiple in-memory stores

(in-package #:ariadne)

(defclass partitioned-graph ()
  ((name :initarg :name :initform nil :accessor partitioned-graph-name)
   (parts :initarg :parts :accessor partitioned-graph-parts)))

(defun make-partitioned-graph (&key name (partitions 4))
  "Create a partitioned graph with N partition stores."
  (make-instance 'partitioned-graph
    :name name
    :parts (let ((v (make-array partitions)))
             (dotimes (i partitions v)
               (setf (aref v i) (make-graph :name (format nil "~A/~A" (or name "pg") i)))))))

(defun partition-count (pg)
  (length (partitioned-graph-parts pg)))

(defun graph-partitions (pg)
  "Return the list of partition graphs."
  (coerce (partitioned-graph-parts pg) 'list))

(defun partition-for (pg subject)
  "Return the partition graph for SUBJECT."
  (let ((idx (mod (sxhash subject) (partition-count pg))))
    (aref (partitioned-graph-parts pg) idx)))

;;; Methods for partitioned-graph

(defmethod add-triple ((pg partitioned-graph) subject predicate object &key graph-name)
  (declare (ignore graph-name))
  (add-triple (partition-for pg subject) subject predicate object))

(defmethod remove-triple ((pg partitioned-graph) subject predicate object)
  (remove-triple (partition-for pg subject) subject predicate object))

(defmethod has-triple-p ((pg partitioned-graph) subject predicate object)
  (has-triple-p (partition-for pg subject) subject predicate object))

(defmethod triple-count ((pg partitioned-graph) &key snapshot)
  (declare (ignore snapshot))
  (reduce #'+ (partitioned-graph-parts pg) :key #'triple-count))

(defmethod get-triples ((pg partitioned-graph) &key subject predicate object)
  (if subject
      (get-triples (partition-for pg subject)
                   :subject subject :predicate predicate :object object)
      (let ((results nil))
        (map nil (lambda (g)
                   (setf results
                         (nconc results
                                (get-triples g :predicate predicate :object object))))
             (partitioned-graph-parts pg))
        results)))

(defmethod query ((pg partitioned-graph) expr)
  (let ((all nil))
    (map nil (lambda (g)
               (let ((r (query g expr)))
                 (when (listp r) (setf all (nconc all r)))))
         (partitioned-graph-parts pg))
    all))
