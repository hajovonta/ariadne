;;;; txlog.lisp
;;;; Append-only transaction log for incremental persistence

(in-package #:ariadne)

(defun graph-txlog-stream (g)
  (getf (graph-extra g) :txlog-stream))

(defun (setf graph-txlog-stream) (val g)
  (setf (getf (graph-extra g) :txlog-stream) val))

(defun start-txlog (g path)
  "Start logging all mutations to PATH. Appends to existing log."
  (bt:with-lock-held ((graph-lock g))
    (let ((stream (open path :direction :output
                             :if-exists :append
                             :if-does-not-exist :create)))
      (setf (graph-txlog-stream g) stream))))

(defun stop-txlog (g)
  "Stop logging and close the log file."
  (bt:with-lock-held ((graph-lock g))
    (let ((stream (graph-txlog-stream g)))
      (when stream
        (force-output stream)
        (close stream)
        (setf (graph-txlog-stream g) nil)))))

(defun txlog-write (g op s p o)
  "Write a log entry if txlog is active."
  (let ((stream (graph-txlog-stream g)))
    (when stream
      (prin1 (list op s p o) stream)
      (terpri stream)
      (force-output stream))))

(defun replay-txlog (g path)
  "Replay a transaction log file into graph G."
  (with-open-file (s path :direction :input :if-does-not-exist nil)
    (when s
      (loop for entry = (read s nil nil)
            while entry do
            (destructuring-bind (op subj pred obj) entry
              (case op
                (:add (add-triple g subj pred obj))
                (:remove (remove-triple g subj pred obj))))))))
