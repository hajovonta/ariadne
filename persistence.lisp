;;;; persistence.lisp
;;;; Save/load graph to/from disk using CL's print/read

(in-package #:ariadne)

(defun save-graph (g path)
  "Save graph to disk."
  (with-open-file (s path :direction :output :if-exists :supersede)
    (let ((*print-readably* t) (*print-circle* nil))
      (print (list :name (graph-name g)
                   :triples (mapcar (lambda (tr)
                                      (list (triple-subject tr)
                                            (triple-predicate tr)
                                            (triple-object tr)))
                                    (get-triples g)))
             s))))

(defun load-graph (path)
  "Load graph from disk."
  (let ((data (with-open-file (s path :direction :input)
                (read s))))
    (let ((g (make-graph :name (getf data :name))))
      (dolist (tr (getf data :triples))
        (add-triple g (first tr) (second tr) (third tr)))
      g)))
