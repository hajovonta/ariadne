# Ariadne Benchmark Results
# Date: 2026-04-09
# System: SBCL 2.2.9, Linux, in-memory
# No optimizations (no string interning, no streaming)

## Turtle Import (real-world ontologies)

| Dataset              | Source     | Triples | Time (s) | Triples/sec | Query (all subjects) |
|----------------------|------------|---------|----------|-------------|---------------------|
| vcard-ontology.ttl   | W3C        |     811 |    0.001 |     811,000 | <1ms                |
| w3c-org.ttl          | W3C        |     729 |    0.003 |     243,000 | <1ms                |
| schema-org.ttl       | Schema.org |  17,949 |    0.036 |     499,000 | 5ms                 |
| brick.ttl            | BrickSchema|  40,507 |    0.081 |     500,000 | 9ms                 |

## N-Triples Import (real-world data)

| Dataset              | Source     | Triples | Time (s) | Triples/sec | Query (all subjects) |
|----------------------|------------|---------|----------|-------------|---------------------|
| berlin.nt            | DBpedia    |   1,861 |    0.005 |     372,200 | <1ms                |
| dbpedia-cities.nt    | DBpedia    |  31,027 |    0.086 |     360,783 | 8ms                 |

## N-Quads Import (Bio2RDF drug database)

| Dataset              | Source     | Lines   | Triples | Time (s) | Triples/sec | GC %  | Memory   |
|----------------------|------------|---------|---------|----------|-------------|-------|----------|
| clinicaltrials-stats | Bio2RDF    |  39,938 |  30,032 |    0.367 |      81,800 |   2%  |   451MB  |
| drugbank-100k.nq     | Bio2RDF    | 100,000 |  89,075 |    0.796 |     112,000 |  14%  |   841MB  |
| drugbank-500k.nq     | Bio2RDF    | 500,000 | 435,412 |    5.052 |      86,000 |  25%  | 4,136MB  |
| drugbank-full.nq     | Bio2RDF    |4,215,954|     OOM |      OOM |         OOM |  OOM  |   OOM    |

## Observations

1. Turtle import: ~500K triples/sec, consistent across 800-40K triples
2. N-Triples import: ~360K triples/sec (longer URIs = more string processing)
3. N-Quads import: ~80-112K triples/sec on Bio2RDF data (very long URIs)
4. Throughput degrades at scale: 112K → 86K triples/sec (100K → 500K triples)
5. GC pressure grows: 14% → 25% of time at 500K triples
6. OOM at ~4.2M lines (979MB file) — need streaming import
7. String allocation is the primary bottleneck — no URI interning

## Bottlenecks (in priority order)

1. uiop:read-file-string loads entire file into memory — need streaming
2. No string interning — repeated URIs create millions of duplicate strings
3. GC pressure from string allocation grows with dataset size
4. Hash table resizing at scale (not yet measured independently)

## Next Steps

- Implement streaming line-by-line import for large files
- Add string interning for URIs (shared string table)
- Re-benchmark after optimizations
- Target: 1M+ triples without OOM, <30s for drugbank-full

## Streaming Import Results (2026-04-09)

| Method              | Lines   | Triples | Time (s) | Triples/sec | GC %  | Memory  |
|---------------------|---------|---------|----------|-------------|-------|---------|
| Bulk (500K lines)   | 500,000 | 435,412 |    5.052 |      86,000 |  25%  | 4,137MB |
| Stream (500K lines) | 500,000 | 435,412 |    3.444 |     126,000 |  35%  | 2,986MB |
| Stream (4.2M lines) |4,215,954|     OOM |      OOM |         OOM |  OOM  |   OOM   |

Streaming is 47% faster and uses 28% less memory than bulk import.
Full drugbank (4.2M lines) still OOMs — in-memory graph too large for default SBCL heap.
Next: string interning to reduce memory, or increase --dynamic-space-size.
