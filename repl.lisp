;;;; repl.lisp
;;;; REPL result formatting

(in-package #:ariadne)

(defun format-results (results &key vars (max-width 60) (stream nil))
  "Format query results as an aligned table. Returns string if STREAM is nil."
  (if (null results)
      (let ((s (format nil "~%0 results~%")))
        (if stream (write-string s stream) s))
      (let* ((headers (mapcar (lambda (v) (string-upcase (princ-to-string v))) vars))
             (ncols (length (first results)))
             (col-headers (or headers
                              (loop for i from 1 to ncols collect (format nil "COL~A" i))))
             ;; Convert all values to truncated strings
             (str-rows (mapcar (lambda (row)
                                 (mapcar (lambda (v) (truncate-value v max-width)) row))
                               results))
             ;; Compute column widths
             (widths (loop for i below ncols
                           collect (max (length (nth i col-headers))
                                        (loop for row in str-rows
                                              maximize (length (nth i row)))))))
        (with-output-to-string (out)
          ;; Header
          (format out "~%")
          (emit-row out col-headers widths)
          (emit-separator out widths)
          ;; Rows
          (dolist (row str-rows)
            (emit-row out row widths))
          ;; Footer
          (format out "~A result~:P~%~%" (length results))
          (when stream
            (write-string (get-output-stream-string out) stream))))))

(defun truncate-value (val max-width)
  (let ((s (princ-to-string val)))
    (if (> (length s) max-width)
        (concatenate 'string (subseq s 0 (- max-width 3)) "...")
        s)))

(defun emit-row (stream cells widths)
  (format stream "| ")
  (loop for cell in cells
        for w in widths
        do (format stream "~vA | " w cell))
  (format stream "~%"))

(defun emit-separator (stream widths)
  (format stream "|")
  (dolist (w widths)
    (format stream "-~v,,,'-A-|" w ""))
  (format stream "~%"))
