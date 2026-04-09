;;;; streaming.lisp
;;;; Line-by-line streaming import for large files

(in-package #:ariadne)

(defun stream-import-ntriples (g path)
  "Import N-Triples from file, reading line by line (constant memory)."
  (with-open-file (s path :direction :input)
    (loop for line = (read-line s nil nil)
          while line do
          (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
            (when (and (> (length trimmed) 0)
                       (char/= #\# (char trimmed 0)))
              (multiple-value-bind (subj pred obj)
                  (parse-ntriple-line trimmed)
                (when (and subj pred obj)
                  (add-triple g subj pred obj))))))))

(defun stream-import-nquads (g path)
  "Import N-Quads from file, reading line by line (constant memory)."
  ;; N-Quads are N-Triples with an optional 4th element
  (stream-import-ntriples g path))
