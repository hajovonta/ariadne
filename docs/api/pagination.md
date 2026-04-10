# Pagination

Cursor-based iteration over large query result sets.

## `make-query-cursor`

```lisp
(make-query-cursor graph expr &key page-size)
```

Create a cursor for paginated query results. Default page-size is 100.

## `cursor-next`

```lisp
(cursor-next cursor)
```

Return the next page of results.

## `cursor-done-p`

```lisp
(cursor-done-p cursor)
```

Return T if all results have been consumed.

### Example

```lisp
(let ((cursor (make-query-cursor g '(select (?s ?p ?o) (where (?s ?p ?o))) :page-size 10)))
  (loop until (cursor-done-p cursor)
        for page = (cursor-next cursor)
        do (format t "Page: ~A results~%" (length page))))
```
