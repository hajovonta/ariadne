;;;; text-search.lisp
;;;; Full-text search over graph literals

(in-package #:ariadne)

(defun tokenize-text (str)
  "Split string into lowercase word tokens."
  (let ((words nil) (start nil))
    (dotimes (i (length str) (when start (push (string-downcase (subseq str start)) words)))
      (if (alphanumericp (char str i))
          (unless start (setf start i))
          (when start
            (push (string-downcase (subseq str start i)) words)
            (setf start nil))))
    (nreverse words)))

(defun graph-text-index (g)
  "Return the text index for graph G, or NIL."
  (getf (graph-extra g) :text-index))

(defun build-text-index (g &key (fields :objects))
  "Build an inverted index over string literals in G.
FIELDS can be :objects (default) or :all (subjects + objects)."
  (let ((index (make-hash-table :test 'equal)))
    (dolist (tr (get-triples g))
      (let ((texts (if (eq fields :all)
                       (list (triple-subject tr) (triple-object tr))
                       (list (triple-object tr)))))
        (dolist (val texts)
          (when (stringp val)
            (dolist (word (tokenize-text val))
              (pushnew tr (gethash word index) :test #'eq))))))
    (setf (getf (graph-extra g) :text-index) index)
    index))

(defun text-search (g query)
  "Search for triples matching all words in QUERY. Requires build-text-index first."
  (let ((index (graph-text-index g)))
    (unless index (error "No text index. Call build-text-index first."))
    (let ((words (tokenize-text query)))
      (when words
        (let ((result (copy-list (gethash (first words) index))))
          (dolist (word (rest words) result)
            (let ((matches (gethash word index)))
              (setf result (intersection result matches :test #'eq)))))))))
