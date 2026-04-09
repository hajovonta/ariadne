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


;;; ==========================================================================
;;; Backup / Restore with versioning
;;; ==========================================================================

(defvar *backup-counter* 0)

(defun backup-graph (g directory)
  "Save a timestamped backup of G to DIRECTORY. Returns the backup path."
  (ensure-directories-exist (merge-pathnames "x" directory))
  (let* ((name (or (graph-name g) "graph"))
         (timestamp (multiple-value-bind (sec min hour day month year)
                        (get-decoded-time)
                      (format nil "~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D-~3,'0D"
                              year month day hour min sec (incf *backup-counter*))))
         (filename (format nil "~A-~A.ariadne" name timestamp))
         (path (merge-pathnames filename directory)))
    (save-graph g path)
    path))

(defun list-backups (directory)
  "List all backup files in DIRECTORY, newest first."
  (let ((files (directory (merge-pathnames "*.ariadne" directory))))
    (sort files #'string> :key #'namestring)))

(defun restore-latest-backup (directory)
  "Load the most recent backup from DIRECTORY."
  (let ((backups (list-backups directory)))
    (if backups
        (load-graph (first backups))
        (error "No backups found in ~A" directory))))
