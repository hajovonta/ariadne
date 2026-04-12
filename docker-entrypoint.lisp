;;;; docker-entrypoint.lisp
;;;; Start Ariadne with SPARQL endpoint inside Docker

(ql:quickload :ariadne :silent t)
(in-package :ariadne)

(defvar *g* (make-graph :name "default"))

;; Load data from /data if mounted
(let ((data-dir #P"/data/"))
  (when (probe-file data-dir)
    (dolist (file (directory (merge-pathnames "*.ttl" data-dir)))
      (handler-case
          (progn
            (import-turtle *g* (uiop:read-file-string file))
            (format t "Loaded ~A (~A triples)~%" (file-namestring file) (triple-count *g*)))
        (error (e) (format t "Error loading ~A: ~A~%" (file-namestring file) e))))
    (dolist (file (directory (merge-pathnames "*.nt" data-dir)))
      (handler-case
          (progn
            (import-ntriples *g* (uiop:read-file-string file))
            (format t "Loaded ~A (~A triples)~%" (file-namestring file) (triple-count *g*)))
        (error (e) (format t "Error loading ~A: ~A~%" (file-namestring file) e))))))

(format t "~%Graph: ~A triples~%" (triple-count *g*))
(start-web-server *g* :port 8080)

;; Keep alive
(loop (sleep 3600))
