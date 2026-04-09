;;;; ariadne.lisp
;;;; Core triple store with SPO/POS/OSP indexing

(in-package #:ariadne)

;;; ==========================================================================
;;; String Interning
;;; ==========================================================================

(defvar *intern-table* (make-hash-table :test 'equal))
(defvar *intern-lock* (bt:make-lock "intern-lock"))

(defun intern-string (s)
  "Return a shared copy of string S, deduplicating repeated strings."
  (bt:with-lock-held (*intern-lock*)
    (or (gethash s *intern-table*)
        (setf (gethash s *intern-table*) s))))

(defun clear-intern-table ()
  (bt:with-lock-held (*intern-lock*)
    (clrhash *intern-table*)))

;;; ==========================================================================
;;; Triple
;;; ==========================================================================

(defstruct (triple (:constructor %make-triple (subject predicate object)))
  subject predicate object)

;;; ==========================================================================
;;; Graph
;;; ==========================================================================

(defstruct (graph (:constructor %make-graph)
                  (:copier nil))
  (name nil)
  (spo (make-hash-table :test 'equal) :type hash-table)
  (sp  (make-hash-table :test 'equal) :type hash-table)
  (s   (make-hash-table :test 'equal) :type hash-table)
  (p   (make-hash-table :test 'equal) :type hash-table)
  (po  (make-hash-table :test 'equal) :type hash-table)
  (o   (make-hash-table :test 'equal) :type hash-table)
  (os  (make-hash-table :test 'equal) :type hash-table)
  (all nil :type list)
  (count 0 :type fixnum)
  (extra nil :type list)
  (triple-graph (make-hash-table :test 'equal) :type hash-table)
  (graph-index (make-hash-table :test 'equal) :type hash-table)
  (lock (bt:make-lock "graph-lock")))

(defun make-graph (&key name)
  (%make-graph :name name))

(defun triplep (x) (triple-p x))

(defun graphp (x) (graph-p x))

(defun triple-count (g &key snapshot)
  (declare (ignore snapshot))
  (graph-count g))

;;; ==========================================================================
;;; Index helpers
;;; ==========================================================================

(defun index-push (ht key triple)
  (push triple (gethash key ht)))

(defun index-delete (ht key triple)
  (setf (gethash key ht) (delete triple (gethash key ht) :test #'eq))
  (when (null (gethash key ht))
    (remhash key ht)))

;;; ==========================================================================
;;; Add / Remove / Query
;;; ==========================================================================

(declaim (ftype function check-triggers))
(declaim (ftype function txlog-write))

(defun add-triple (g subject predicate object)
  (when (or (null subject) (null predicate))
    (error "Subject and predicate must not be NIL"))
  (when (stringp subject) (setf subject (intern-string subject)))
  (when (stringp predicate) (setf predicate (intern-string predicate)))
  (when (stringp object) (setf object (intern-string object)))
  (bt:with-lock-held ((graph-lock g))
    (let ((key (list subject predicate object)))
      (when (gethash key (graph-spo g))
        (return-from add-triple (gethash key (graph-spo g))))
      (let ((tr (%make-triple subject predicate object)))
        (setf (gethash key (graph-spo g)) tr)
        (index-push (graph-sp g) (list subject predicate) tr)
        (index-push (graph-s g) subject tr)
        (index-push (graph-p g) predicate tr)
        (index-push (graph-po g) (list predicate object) tr)
        (index-push (graph-o g) object tr)
        (index-push (graph-os g) (list object subject) tr)
        (push tr (graph-all g))
        (incf (graph-count g))
        (when (graph-triggers g)
          (check-triggers g tr))
        (txlog-write g :add subject predicate object)
        tr))))

(defun remove-triple (g subject predicate object)
  (bt:with-lock-held ((graph-lock g))
    (let* ((key (list subject predicate object))
           (tr (gethash key (graph-spo g))))
      (when tr
        (remhash key (graph-spo g))
        (index-delete (graph-sp g) (list subject predicate) tr)
        (index-delete (graph-s g) subject tr)
        (index-delete (graph-p g) predicate tr)
        (index-delete (graph-po g) (list predicate object) tr)
        (index-delete (graph-o g) object tr)
        (index-delete (graph-os g) (list object subject) tr)
        (setf (graph-all g) (delete tr (graph-all g) :test #'eq))
        (decf (graph-count g))
        (txlog-write g :remove subject predicate object)
        t))))

(defun remove-triples (g &key subject predicate object)
  "Remove all triples matching the given constraints."
  (dolist (tr (get-triples g :subject subject :predicate predicate :object object))
    (remove-triple g (triple-subject tr) (triple-predicate tr) (triple-object tr))))

(defun get-triples (g &key subject predicate object)
  "Query triples using the best flat index."
  (cond
    ((and subject predicate object)
     (let ((tr (gethash (list subject predicate object) (graph-spo g))))
       (when tr (list tr))))
    ((and subject predicate)
     (copy-list (gethash (list subject predicate) (graph-sp g))))
    ((and predicate object)
     (copy-list (gethash (list predicate object) (graph-po g))))
    ((and object subject)
     (copy-list (gethash (list object subject) (graph-os g))))
    (subject (copy-list (gethash subject (graph-s g))))
    (predicate (copy-list (gethash predicate (graph-p g))))
    (object (copy-list (gethash object (graph-o g))))
    (t (copy-list (graph-all g)))))

(defun has-triple-p (g subject predicate object)
  (not (null (gethash (list subject predicate object) (graph-spo g)))))

(defun clear-graph (g)
  (clrhash (graph-spo g))
  (clrhash (graph-sp g))
  (clrhash (graph-s g))
  (clrhash (graph-p g))
  (clrhash (graph-po g))
  (clrhash (graph-o g))
  (clrhash (graph-os g))
  (setf (graph-all g) nil)
  (setf (graph-count g) 0))

;;; ==========================================================================
;;; Enumeration
;;; ==========================================================================

(defun collect-keys (ht)
  (let (keys) (maphash (lambda (k v) (declare (ignore v)) (push k keys)) ht) keys))

(defun all-subjects (g) (collect-keys (graph-s g)))
(defun all-predicates (g) (collect-keys (graph-p g)))
(defun all-objects (g) (collect-keys (graph-o g)))

;;; ==========================================================================
;;; Named Graphs (Quads)
;;; ==========================================================================

(defun add-quad (g subject predicate object graph-name)
  "Add a triple associated with a named graph."
  (let ((tr (add-triple g subject predicate object)))
    (when graph-name
      (let ((key (list subject predicate object)))
        (setf (gethash key (graph-triple-graph g)) graph-name)
        (pushnew key (gethash graph-name (graph-graph-index g)) :test #'equal)))
    tr))

(defun get-quads (g &key graph subject predicate object)
  "Query triples, optionally filtered by graph name."
  (if graph
      (let ((keys (gethash graph (graph-graph-index g)))
            (results nil))
        (dolist (key keys results)
          (destructuring-bind (s p o) key
            (when (and (or (null subject) (equal subject s))
                       (or (null predicate) (equal predicate p))
                       (or (null object) (equal object o)))
              (let ((trs (get-triples g :subject s :predicate p :object o)))
                (dolist (tr trs) (push tr results)))))))
      (get-triples g :subject subject :predicate predicate :object object)))

(defun named-graphs (g)
  "List all named graph URIs."
  (collect-keys (graph-graph-index g)))
