;;;; tests/suite-full-text-search.lisp
;;;; Tests for full-text search over graph literals

(in-package #:ariadne/tests)

(in-suite :full-text-search)

(test build-text-index
  "build-text-index creates a searchable index"
  (let ((g (make-graph :name "fts-test")))
    (add-triple g "http://ex.org/alice" "http://ex.org/name" "Alice Smith")
    (add-triple g "http://ex.org/bob" "http://ex.org/name" "Bob Jones")
    (build-text-index g)
    (is-true (graph-text-index g))))

(test text-search-basic
  "text-search finds triples by keyword"
  (let ((g (make-graph :name "fts-basic")))
    (add-triple g "http://ex.org/alice" "http://ex.org/name" "Alice Smith")
    (add-triple g "http://ex.org/bob" "http://ex.org/name" "Bob Jones")
    (add-triple g "http://ex.org/carol" "http://ex.org/name" "Carol Smith")
    (build-text-index g)
    (let ((results (text-search g "smith")))
      (is (= 2 (length results)))
      (is (every (lambda (tr) (search "Smith" (triple-object tr))) results)))))

(test text-search-case-insensitive
  "text-search is case insensitive"
  (let ((g (make-graph :name "fts-case")))
    (add-triple g "http://ex.org/a" "http://ex.org/desc" "The Quick Brown Fox")
    (build-text-index g)
    (is (= 1 (length (text-search g "QUICK"))))
    (is (= 1 (length (text-search g "quick"))))
    (is (= 1 (length (text-search g "Quick"))))))

(test text-search-multiple-words
  "text-search with multiple words finds triples containing all words"
  (let ((g (make-graph :name "fts-multi")))
    (add-triple g "http://ex.org/a" "http://ex.org/desc" "Common Lisp programming")
    (add-triple g "http://ex.org/b" "http://ex.org/desc" "Common sense")
    (add-triple g "http://ex.org/c" "http://ex.org/desc" "Lisp machine")
    (build-text-index g)
    (let ((results (text-search g "common lisp")))
      (is (= 1 (length results)))
      (is (string= "Common Lisp programming" (triple-object (first results)))))))

(test text-search-no-match
  "text-search returns nil when no match"
  (let ((g (make-graph :name "fts-none")))
    (add-triple g "http://ex.org/a" "http://ex.org/name" "Alice")
    (build-text-index g)
    (is (null (text-search g "zebra")))))

(test text-search-non-string-objects
  "text-search ignores non-string objects"
  (let ((g (make-graph :name "fts-nonstr")))
    (add-triple g "http://ex.org/a" "http://ex.org/age" 42)
    (add-triple g "http://ex.org/a" "http://ex.org/name" "Alice")
    (build-text-index g)
    (is (= 1 (length (text-search g "alice"))))))

(test text-search-subjects-too
  "text-search with :fields :all searches subjects and objects"
  (let ((g (make-graph :name "fts-subj")))
    (add-triple g "http://example.org/graph-database" "http://ex.org/type" "software")
    (build-text-index g :fields :all)
    (is (= 1 (length (text-search g "graph-database"))))))
