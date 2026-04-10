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

(defstruct (triple (:constructor %make-triple (subject predicate object &optional graph)))
  subject predicate object graph)

;;; ==========================================================================
;;; Graph
;;; ==========================================================================

(defclass graph ()
  ((name :initarg :name :initform nil :accessor graph-name)
   (spo :initform (make-hash-table :test 'equal) :accessor graph-spo)
   (sp  :initform (make-hash-table :test 'equal) :accessor graph-sp)
   (s   :initform (make-hash-table :test 'equal) :accessor graph-s)
   (p   :initform (make-hash-table :test 'equal) :accessor graph-p)
   (po  :initform (make-hash-table :test 'equal) :accessor graph-po)
   (o   :initform (make-hash-table :test 'equal) :accessor graph-o)
   (os  :initform (make-hash-table :test 'equal) :accessor graph-os)
   (all :initform nil :accessor graph-all)
   (count :initform 0 :accessor graph-count)
   (extra :initform nil :accessor graph-extra)
   (triple-graph :initform (make-hash-table :test 'equal) :accessor graph-triple-graph)
   (graph-index :initform (make-hash-table :test 'equal) :accessor graph-graph-index)
   (lock :initform (bt:make-lock "graph-lock") :accessor graph-lock)))

(defun make-graph (&key name)
  (make-instance 'graph :name name))

(defun triplep (x) (triple-p x))

(defun graphp (x) (typep x 'graph))

;;; ==========================================================================
;;; Generic functions for polymorphism
;;; ==========================================================================

(defgeneric triple-count (g &key snapshot))
(defgeneric add-triple (g subject predicate object &key graph-name))
(defgeneric remove-triple (g subject predicate object))
(defgeneric get-triples (g &key subject predicate object))
(defgeneric has-triple-p (g subject predicate object))
(defgeneric clear-graph (g))
(defgeneric query (g expr))

(defmethod triple-count ((g graph) &key snapshot)
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
(declaim (ftype function expand-if-prefixed))
(declaim (ftype function fire-graph-events))

(defmethod add-triple ((g graph) subject predicate object &key graph-name)
  (when (or (null subject) (null predicate))
    (error "Subject and predicate must not be NIL"))
  (when (stringp subject) (setf subject (intern-string (expand-if-prefixed g subject))))
  (when (stringp predicate) (setf predicate (intern-string (expand-if-prefixed g predicate))))
  (when (stringp object) (setf object (intern-string (expand-if-prefixed g object))))
  (bt:with-lock-held ((graph-lock g))
    (let ((key (list subject predicate object)))
      (when (gethash key (graph-spo g))
        (let ((existing (gethash key (graph-spo g))))
          (when graph-name
            (pushnew existing (gethash graph-name (graph-graph-index g))))
          (return-from add-triple existing)))
      (let ((tr (%make-triple subject predicate object graph-name)))
        (setf (gethash key (graph-spo g)) tr)
        (index-push (graph-sp g) (list subject predicate) tr)
        (index-push (graph-s g) subject tr)
        (index-push (graph-p g) predicate tr)
        (index-push (graph-po g) (list predicate object) tr)
        (index-push (graph-o g) object tr)
        (index-push (graph-os g) (list object subject) tr)
        (push tr (graph-all g))
        (incf (graph-count g))
        (when graph-name
          (push tr (gethash graph-name (graph-graph-index g))))
        (when (graph-triggers g)
          (check-triggers g tr))
        (txlog-write g :add subject predicate object)
        (fire-graph-events g :add subject predicate object)
        tr))))

(defmethod remove-triple ((g graph) subject predicate object)
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
        (fire-graph-events g :remove subject predicate object)
        t))))

(defun remove-triples (g &key subject predicate object)
  "Remove all triples matching the given constraints."
  (dolist (tr (get-triples g :subject subject :predicate predicate :object object))
    (remove-triple g (triple-subject tr) (triple-predicate tr) (triple-object tr))))

(defmethod get-triples ((g graph) &key subject predicate object)
  "Query triples using the best flat index."
  (when (stringp subject) (setf subject (expand-if-prefixed g subject)))
  (when (stringp predicate) (setf predicate (expand-if-prefixed g predicate)))
  (when (stringp object) (setf object (expand-if-prefixed g object)))
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

(defmethod has-triple-p ((g graph) subject predicate object)
  (when (stringp subject) (setf subject (expand-if-prefixed g subject)))
  (when (stringp predicate) (setf predicate (expand-if-prefixed g predicate)))
  (when (stringp object) (setf object (expand-if-prefixed g object)))
  (not (null (gethash (list subject predicate object) (graph-spo g)))))

(defmethod clear-graph ((g graph))
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
  (add-triple g subject predicate object :graph-name graph-name))

(defun get-quads (g &key graph subject predicate object)
  "Query triples, optionally filtered by graph name."
  (if graph
      (remove-if-not
       (lambda (tr)
         (and (or (null subject) (equal subject (triple-subject tr)))
              (or (null predicate) (equal predicate (triple-predicate tr)))
              (or (null object) (equal object (triple-object tr)))))
       (gethash graph (graph-graph-index g)))
      (get-triples g :subject subject :predicate predicate :object object)))

(defun named-graphs (g)
  "List all named graph URIs."
  (collect-keys (graph-graph-index g)))
