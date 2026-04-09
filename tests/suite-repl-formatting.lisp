;;;; tests/suite-repl-formatting.lisp
;;;; REPL result formatting: pretty-print query results as tables

(in-package #:ariadne/tests)
(in-suite :repl-formatting)

;; =============================================================================
;; Table Formatting
;; =============================================================================

(test format-results-basic
  "Format query results as an aligned table"
  (let ((g (make-graph)))
    (add-triple g "alice" "age" 30)
    (add-triple g "bob" "age" 25)
    (let* ((results (query g '(select (?person ?age)
                               (where (?person "age" ?age)))))
           (output (format-results results :vars '(?person ?age))))
      (is (stringp output))
      (is (search "?PERSON" output))
      (is (search "?AGE" output))
      (is (search "alice" output))
      (is (search "bob" output)))))

(test format-results-empty
  "Format empty results"
  (let ((output (format-results nil :vars '(?x))))
    (is (stringp output))
    (is (search "0 results" output))))

(test format-results-single-column
  "Format single-column results"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (let* ((results (query g '(select (?who)
                               (where ("alice" "knows" ?who)))))
           (output (format-results results :vars '(?who))))
      (is (search "?WHO" output))
      (is (search "bob" output)))))

(test format-results-count
  "Format results includes row count"
  (let ((g (make-graph)))
    (add-triple g "alice" "knows" "bob")
    (add-triple g "alice" "knows" "charlie")
    (let* ((results (query g '(select (?who)
                               (where ("alice" "knows" ?who)))))
           (output (format-results results :vars '(?who))))
      (is (search "2 results" output)))))

(test format-results-truncates-long-values
  "Long values are truncated in table display"
  (let ((g (make-graph)))
    (add-triple g "http://example.org/very/long/uri/that/goes/on/and/on" "type" "thing")
    (let* ((results (query g '(select (?s) (where (?s "type" "thing")))))
           (output (format-results results :vars '(?s) :max-width 30)))
      (is (stringp output))
      ;; Should contain truncated URI
      (is (search "..." output)))))
