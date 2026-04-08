;;;; transactions.lisp
;;;; Basic transaction support with rollback

(in-package #:ariadne)

(defstruct (transaction (:constructor %make-transaction))
  graph
  (snapshot nil :type list))

(defun begin-transaction (g)
  "Begin a transaction, capturing a snapshot of current triples."
  (%make-transaction :graph g
                     :snapshot (mapcar (lambda (tr)
                                         (list (triple-subject tr)
                                               (triple-predicate tr)
                                               (triple-object tr)))
                                       (get-triples g))))

(defun rollback-transaction (tx)
  "Restore graph to the snapshot taken at begin-transaction."
  (let ((g (transaction-graph tx)))
    (clear-graph g)
    (dolist (tr (transaction-snapshot tx))
      (add-triple g (first tr) (second tr) (third tr)))))

(defmacro with-transaction ((graph) &body body)
  "Execute BODY within a transaction. Rolls back on error."
  (let ((tx (gensym "TX")))
    `(let ((,tx (begin-transaction ,graph)))
       (handler-case
           (progn ,@body)
         (error (e)
           (rollback-transaction ,tx)
           (error e))))))
