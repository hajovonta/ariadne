;;;; ariadne.lisp
;;;; Core triple store with SPO/POS/OSP indexing

(in-package #:ariadne)

;;; ==========================================================================
;;; Triple
;;; ==========================================================================

(defstruct (triple (:constructor %make-triple (subject predicate object)))
  subject predicate object)

;;; ==========================================================================
;;; Graph
;;; ==========================================================================

(defstruct (graph (:constructor %make-graph))
  (name nil)
  (spo (make-hash-table :test 'equal) :type hash-table)
  (pos (make-hash-table :test 'equal) :type hash-table)
  (osp (make-hash-table :test 'equal) :type hash-table)
  (count 0 :type fixnum)
  (extra nil :type list))

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

(defun ensure-nested (ht key)
  "Get or create a nested hash-table at KEY in HT."
  (or (gethash key ht)
      (setf (gethash key ht) (make-hash-table :test 'equal))))

(defun index-add (ht k1 k2 k3 triple)
  (setf (gethash k3 (ensure-nested (ensure-nested ht k1) k2)) triple))

(defun index-remove (ht k1 k2 k3)
  (let ((l1 (gethash k1 ht)))
    (when l1
      (let ((l2 (gethash k2 l1)))
        (when l2
          (remhash k3 l2)
          (when (= 0 (hash-table-count l2))
            (remhash k2 l1)
            (when (= 0 (hash-table-count l1))
              (remhash k1 ht))))))))

(defun index-lookup (ht &optional k1 k2 k3)
  "Collect triples from a 3-level nested hash-table with 0-3 keys bound."
  (let (results)
    (flet ((collect-all (inner)
             (maphash (lambda (k v)
                        (declare (ignore k))
                        (if (triple-p v)
                            (push v results)
                            (maphash (lambda (k2 v2)
                                       (declare (ignore k2))
                                       (if (triple-p v2)
                                           (push v2 results)
                                           (maphash (lambda (k3 v3)
                                                      (declare (ignore k3))
                                                      (push v3 results))
                                                    v2)))
                                     v)))
                      inner)))
      (cond
        ((and k1 k2 k3)
         (let* ((l1 (gethash k1 ht))
                (l2 (and l1 (gethash k2 l1)))
                (tr (and l2 (gethash k3 l2))))
           (when tr (push tr results))))
        ((and k1 k2)
         (let* ((l1 (gethash k1 ht))
                (l2 (and l1 (gethash k2 l1))))
           (when l2 (maphash (lambda (k v) (declare (ignore k)) (push v results)) l2))))
        (k1
         (let ((l1 (gethash k1 ht)))
           (when l1
             (maphash (lambda (k v)
                        (declare (ignore k))
                        (maphash (lambda (k2 v2) (declare (ignore k2)) (push v2 results)) v))
                      l1))))
        (t (collect-all ht))))
    results))

;;; ==========================================================================
;;; Add / Remove / Query
;;; ==========================================================================

(defun add-triple (g subject predicate object)
  (when (or (null subject) (null predicate))
    (error "Subject and predicate must not be NIL"))
  ;; Check for duplicate
  (when (has-triple-p g subject predicate object)
    (return-from add-triple
      (first (index-lookup (graph-spo g) subject predicate object))))
  (let ((tr (%make-triple subject predicate object)))
    (index-add (graph-spo g) subject predicate object tr)
    (index-add (graph-pos g) predicate object subject tr)
    (index-add (graph-osp g) object subject predicate tr)
    (incf (graph-count g))
    tr))

(defun remove-triple (g subject predicate object)
  (when (has-triple-p g subject predicate object)
    (index-remove (graph-spo g) subject predicate object)
    (index-remove (graph-pos g) predicate object subject)
    (index-remove (graph-osp g) object subject predicate)
    (decf (graph-count g))
    t))

(defun remove-triples (g &key subject predicate object)
  "Remove all triples matching the given constraints."
  (dolist (tr (get-triples g :subject subject :predicate predicate :object object))
    (remove-triple g (triple-subject tr) (triple-predicate tr) (triple-object tr))))

(defun get-triples (g &key subject predicate object)
  "Query triples. Uses the best index based on which keys are provided."
  (cond
    ;; Use SPO index when subject is known
    (subject
     (if predicate
         (if object
             (index-lookup (graph-spo g) subject predicate object)
             (index-lookup (graph-spo g) subject predicate))
         (if object
             ;; s + o: use OSP
             (index-lookup (graph-osp g) object subject)
             (index-lookup (graph-spo g) subject))))
    ;; Use POS index when predicate is known
    (predicate
     (if object
         (index-lookup (graph-pos g) predicate object)
         (index-lookup (graph-pos g) predicate)))
    ;; Use OSP index when only object is known
    (object
     (index-lookup (graph-osp g) object))
    ;; No constraints: return all
    (t (index-lookup (graph-spo g)))))

(defun has-triple-p (g subject predicate object)
  (not (null (index-lookup (graph-spo g) subject predicate object))))

(defun clear-graph (g)
  (clrhash (graph-spo g))
  (clrhash (graph-pos g))
  (clrhash (graph-osp g))
  (setf (graph-count g) 0))

;;; ==========================================================================
;;; Enumeration
;;; ==========================================================================

(defun collect-keys (ht)
  (let (keys) (maphash (lambda (k v) (declare (ignore v)) (push k keys)) ht) keys))

(defun all-subjects (g) (collect-keys (graph-spo g)))
(defun all-predicates (g) (collect-keys (graph-pos g)))

(defun all-objects (g) (collect-keys (graph-osp g)))
