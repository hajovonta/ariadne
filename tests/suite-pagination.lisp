;;;; tests/suite-pagination.lisp
;;;; Query result pagination and streaming

(in-package #:ariadne/tests)
(in-suite :pagination)

(test paginated-query
  "Query with pagination returns correct pages"
  (let ((g (make-graph)))
    (dotimes (i 20)
      (add-triple g (format nil "node-~A" i) "type" "node"))
    (let ((page1 (query g '(select (?s) (where (?s "type" "node")) (limit 5))))
          (page2 (query g '(select (?s) (where (?s "type" "node")) (limit 5) (offset 5)))))
      (is (= 5 (length page1)))
      (is (= 5 (length page2)))
      (is (null (intersection page1 page2 :test #'equal))))))

(test query-cursor-basic
  "Create and iterate a query cursor"
  (let ((g (make-graph)))
    (dotimes (i 50)
      (add-triple g (format nil "n~A" i) "type" "node"))
    (let ((cursor (make-query-cursor g '(select (?s) (where (?s "type" "node")))
                                     :page-size 10)))
      (is (= 10 (length (cursor-next cursor))))
      (is (= 10 (length (cursor-next cursor))))
      (is-false (cursor-done-p cursor))
      ;; Consume remaining
      (cursor-next cursor)
      (cursor-next cursor)
      (cursor-next cursor)
      (is (cursor-done-p cursor)))))

(test query-cursor-small-result
  "Cursor with fewer results than page size"
  (let ((g (make-graph)))
    (add-triple g "a" "type" "node")
    (add-triple g "b" "type" "node")
    (let ((cursor (make-query-cursor g '(select (?s) (where (?s "type" "node")))
                                     :page-size 10)))
      (is (= 2 (length (cursor-next cursor))))
      (is (cursor-done-p cursor)))))
