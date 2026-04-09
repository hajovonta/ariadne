;;;; graph-ops.lisp
;;;; Graph operations: merge, diff, copy

(in-package #:ariadne)

(defun merge-graphs (g1 g2)
  "Create a new graph containing all triples from G1 and G2."
  (let ((result (make-graph :name (graph-name g1))))
    (dolist (tr (get-triples g1))
      (add-triple result (triple-subject tr) (triple-predicate tr) (triple-object tr)))
    (dolist (tr (get-triples g2))
      (add-triple result (triple-subject tr) (triple-predicate tr) (triple-object tr)))
    result))

(defun merge-graphs-into (target source)
  "Add all triples from SOURCE into TARGET."
  (dolist (tr (get-triples source))
    (add-triple target (triple-subject tr) (triple-predicate tr) (triple-object tr)))
  target)

(defun diff-graphs (g1 g2)
  "Return triples in G1 that are not in G2."
  (remove-if (lambda (tr)
               (has-triple-p g2 (triple-subject tr) (triple-predicate tr) (triple-object tr)))
             (get-triples g1)))

(defun copy-graph (g)
  "Create an independent deep copy of G."
  (let ((result (make-graph :name (graph-name g))))
    (dolist (tr (get-triples g))
      (add-triple result (triple-subject tr) (triple-predicate tr) (triple-object tr)))
    result))


;;; ==========================================================================
;;; Export: Cytoscape JSON
;;; ==========================================================================

(defun export-cytoscape-json (g &key predicates center depth)
  "Export graph as Cytoscape.js compatible JSON elements array."
  (graph-to-cytoscape-json g :predicates predicates :center center :depth depth))

;;; ==========================================================================
;;; Export: JSON-LD
;;; ==========================================================================

(defun export-json-ld (g)
  "Export graph as JSON-LD string."
  (let ((by-subject (make-hash-table :test 'equal))
        (rdf-type "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
    (dolist (tr (get-triples g))
      (push tr (gethash (triple-subject tr) by-subject)))
    (with-output-to-string (s)
      (write-string "[" s)
      (let ((first-subj t))
        (maphash
         (lambda (subj triples)
           (if first-subj (setf first-subj nil) (write-string "," s))
           (format s "{\"@id\":\"~A\"" (json-escape subj))
           ;; Collect types and properties
           (dolist (tr triples)
             (let ((pred (triple-predicate tr))
                   (obj (triple-object tr)))
               (if (equal pred rdf-type)
                   (format s ",\"@type\":\"~A\"" (json-escape obj))
                   (format s ",\"~A\":~A"
                           (json-escape pred)
                           (json-ld-value obj)))))
           (write-string "}" s))
         by-subject))
      (write-string "]" s))))

(defun json-ld-value (obj)
  (cond
    ((stringp obj)
     (if (and (> (length obj) 0) (search "://" obj))
         (format nil "{\"@id\":\"~A\"}" (json-escape obj))
         (format nil "\"~A\"" (json-escape obj))))
    ((numberp obj) (format nil "~A" obj))
    ((eq obj t) "true")
    ((null obj) "false")
    (t (format nil "\"~A\"" (json-escape (princ-to-string obj))))))


;;; ==========================================================================
;;; JSON-LD Import
;;; ==========================================================================

(defun import-json-ld (g data)
  "Import JSON-LD string into graph G."
  (let ((objects (parse-json-ld-array data))
        (rdf-type "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
    (dolist (obj objects)
      (let ((id (cdr (assoc "@id" obj :test #'equal))))
        (when id
          (dolist (pair obj)
            (let ((key (car pair))
                  (val (cdr pair)))
              (cond
                ((equal key "@id") nil)
                ((equal key "@type")
                 (add-triple g id rdf-type val))
                ((and (consp val) (assoc "@id" val :test #'equal))
                 (add-triple g id key (cdr (assoc "@id" val :test #'equal))))
                (t (add-triple g id key val))))))))))

(defun parse-json-ld-array (str)
  "Minimal JSON array-of-objects parser. Returns list of alists."
  (let ((pos 0) (len (length str)))
    (labels
        ((skip-ws ()
           (loop while (and (< pos len)
                            (member (char str pos) '(#\Space #\Tab #\Newline #\Return)))
                 do (incf pos)))
         (expect (c) (skip-ws) (when (and (< pos len) (char= c (char str pos))) (incf pos) t))
         (read-string ()
           (skip-ws)
           (when (and (< pos len) (char= #\" (char str pos)))
             (incf pos)
             (let ((start pos))
               (loop while (and (< pos len) (char/= #\" (char str pos)))
                     do (when (char= #\\ (char str pos)) (incf pos))
                        (incf pos))
               (prog1 (subseq str start pos) (incf pos)))))
         (read-number ()
           (skip-ws)
           (let ((start pos))
             (loop while (and (< pos len)
                              (or (digit-char-p (char str pos))
                                  (member (char str pos) '(#\. #\- #\+ #\e #\E))))
                   do (incf pos))
             (let ((s (subseq str start pos)))
               (if (find #\. s) (read-from-string s) (parse-integer s)))))
         (read-value ()
           (skip-ws)
           (when (< pos len)
             (case (char str pos)
               (#\" (read-string))
               (#\{ (read-object))
               (#\[ (read-array))
               (t (if (or (digit-char-p (char str pos)) (char= #\- (char str pos)))
                      (read-number)
                      ;; true/false/null
                      (let ((start pos))
                        (loop while (and (< pos len) (alpha-char-p (char str pos))) do (incf pos))
                        (let ((w (subseq str start pos)))
                          (cond ((equal w "true") t) ((equal w "false") nil) (t w)))))))))
         (read-object ()
           (expect #\{)
           (let ((pairs nil))
             (loop do
               (let ((key (read-string)))
                 (when key
                   (expect #\:)
                   (push (cons key (read-value)) pairs)))
                   while (expect #\,))
             (expect #\})
             (nreverse pairs)))
         (read-array ()
           (expect #\[)
           (let ((items nil))
             (loop do (let ((v (read-value))) (when v (push v items)))
                   while (expect #\,))
             (expect #\])
             (nreverse items))))
      (read-array))))

(defun blank-node-p (s)
  "Return T if S is a blank node string."
  (and (stringp s) (>= (length s) 2) (char= (char s 0) #\_) (char= (char s 1) #\:)))

(defun skolemize-blank-nodes (g &key (base "https://ariadne.example"))
  "Replace all blank nodes in G with stable well-known URIs."
  (let ((mapping (make-hash-table :test 'equal))
        (triples (get-triples g)))
    (flet ((skolem-uri (bnode)
             (or (gethash bnode mapping)
                 (setf (gethash bnode mapping)
                       (format nil "~A/.well-known/genid/~A" base (subseq bnode 2))))))
      (dolist (tr triples)
        (let ((s (triple-subject tr))
              (p (triple-predicate tr))
              (o (triple-object tr)))
          (when (or (blank-node-p s) (blank-node-p o))
            (remove-triple g s p o)
            (add-triple g
                        (if (blank-node-p s) (skolem-uri s) s)
                        p
                        (if (blank-node-p o) (skolem-uri o) o)))))))
  g)
