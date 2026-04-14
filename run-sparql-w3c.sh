#!/bin/bash
# Run W3C SPARQL 1.1 tests, one SBCL process per category
cd "$(dirname "$0")"

CATS="aggregates bind bindings cast construct exists functions grouping negation project-expression property-path subquery syntax-query"
TOTAL_P=0 TOTAL_F=0 TOTAL_S=0

for cat in $CATS; do
  echo "=== $cat ==="
  result=$(timeout 180 sbcl --noinform --non-interactive \
    --eval '(require :asdf)' \
    --eval '(asdf:load-system :ariadne :silent t)' \
    --eval '(in-package :ariadne)' \
    --eval '(load "run-sparql-w3c.lisp")' \
    --eval "(multiple-value-bind (p f s) (run-category \"${cat}\") (format t \"~%RESULT: ~A ~A ~A~%\" p f s))" \
    2>/dev/null | grep "^RESULT:")
  if [ -n "$result" ]; then
    p=$(echo "$result" | awk '{print $2}')
    f=$(echo "$result" | awk '{print $3}')
    s=$(echo "$result" | awk '{print $4}')
    total=$((p + f + s))
    echo "  $p pass, $f fail, $s skip (of $total)"
    TOTAL_P=$((TOTAL_P + p))
    TOTAL_F=$((TOTAL_F + f))
    TOTAL_S=$((TOTAL_S + s))
  else
    echo "  CRASHED/TIMEOUT"
  fi
done

TOTAL=$((TOTAL_P + TOTAL_F + TOTAL_S))
PCT=$(echo "scale=1; $TOTAL_P * 100 / ($TOTAL_P + $TOTAL_F)" | bc)
echo ""
echo "=== TOTAL: $TOTAL_P pass, $TOTAL_F fail, $TOTAL_S skip (of $TOTAL) — ${PCT}% of runnable ==="
