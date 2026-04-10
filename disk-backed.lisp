;;;; disk-backed.lisp
;;;; Disk-backed persistent graph storage

(in-package #:ariadne)

(defun open-disk-graph (path &key name)
  "Open a disk-backed graph. Replays existing data, then appends new mutations."
  (let ((g (make-graph :name name)))
    ;; Replay existing data
    (when (probe-file path)
      (replay-txlog g path))
    ;; Open stream for appending
    (let ((stream (open path :direction :output
                             :if-exists :append
                             :if-does-not-exist :create)))
      (setf (graph-txlog-stream g) stream)
      (setf (getf (graph-extra g) :disk-stream) stream)
      (setf (getf (graph-extra g) :disk-path) path))
    g))

(defun close-disk-graph (g)
  "Close a disk-backed graph, flushing pending writes."
  (stop-txlog g)
  (setf (getf (graph-extra g) :disk-stream) nil))

(defun compact-disk-graph (g)
  "Rewrite the backing file with only live triples, removing deleted entries."
  (let ((path (getf (graph-extra g) :disk-path)))
    (unless path (error "Not a disk-backed graph"))
    ;; Close current stream
    (stop-txlog g)
    ;; Rewrite file
    (with-open-file (s path :direction :output :if-exists :supersede)
      (dolist (tr (get-triples g))
        (prin1 (list :add (triple-subject tr) (triple-predicate tr) (triple-object tr)) s)
        (terpri s)))
    ;; Reopen for appending
    (let ((stream (open path :direction :output
                             :if-exists :append
                             :if-does-not-exist :create)))
      (setf (graph-txlog-stream g) stream)
      (setf (getf (graph-extra g) :disk-stream) stream))))
