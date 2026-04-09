;;;; tests/suite-backup.lisp
;;;; Backup/restore with versioning

(in-package #:ariadne/tests)
(in-suite :backup)

(defparameter *test-backup-dir*
  (merge-pathnames "test-data/backups/"
                   (asdf:system-source-directory :ariadne-tests)))

(defun cleanup-test-backup-dir ()
  (when (probe-file *test-backup-dir*)
    (dolist (f (directory (merge-pathnames "*.*" *test-backup-dir*)))
      (delete-file f))
    (ignore-errors (uiop:delete-empty-directory *test-backup-dir*))))

(test backup-creates-versioned-file
  "Backup creates a timestamped file"
  (let ((g (make-graph :name "test")))
    (add-triple g "alice" "knows" "bob")
    (unwind-protect
         (let ((path (backup-graph g *test-backup-dir*)))
           (is (probe-file path))
           (is (search "test" (namestring path))))
      (cleanup-test-backup-dir))))

(test backup-restore-latest
  "Restore latest backup"
  (let ((g (make-graph :name "test")))
    (add-triple g "alice" "knows" "bob")
    (unwind-protect
         (progn
           (backup-graph g *test-backup-dir*)
           (add-triple g "bob" "knows" "charlie")
           (backup-graph g *test-backup-dir*)
           (let ((g2 (restore-latest-backup *test-backup-dir*)))
             (is (= 2 (triple-count g2)))))
      (cleanup-test-backup-dir))))

(test list-backups
  "List available backups"
  (cleanup-test-backup-dir)
  (let ((g (make-graph :name "test")))
    (add-triple g "a" "b" "c")
    (unwind-protect
         (progn
           (backup-graph g *test-backup-dir*)
           (backup-graph g *test-backup-dir*)
           (is (= 2 (length (list-backups *test-backup-dir*)))))
      (cleanup-test-backup-dir))))
