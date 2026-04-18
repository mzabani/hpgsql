hpgsql's repository contains a small benchmark suite that compares hpgsql to postgresql-simple, hasql, and streaming-postgresql-simple.

I'm no expert using some of these libraries, so I welcome scrutiny and contributions.

You can clone hpgsql and if you have Nix, you should be able to run this with `run benchmarks`.

Output of `run benchmarks` on my computer:

```
IMPORTANT: all measurements collected over 10 runs of each benchmark
--- Benchmark hpgsql Tuple List (10000 rows): Wall time=340.2 ms, total Haskell (does not include C heap) memory allocated=715.1 MB.
--- Benchmark hasql Tuple List (10000 rows): Wall time=662.3 ms, total Haskell (does not include C heap) memory allocated=491.4 MB.
--- Benchmark postgresql-simple Tuple List (10000 rows): Wall time=1.211 s, total Haskell (does not include C heap) memory allocated=2145.6 MB.
--- Benchmark hpgsql Record List (10000 rows): Wall time=253.7 ms, total Haskell (does not include C heap) memory allocated=745.5 MB.
--- Benchmark hasql Record List (10000 rows): Wall time=596.6 ms, total Haskell (does not include C heap) memory allocated=489.8 MB.
--- Benchmark postgresql-simple Record List (10000 rows): Wall time=1.111 s, total Haskell (does not include C heap) memory allocated=2040.3 MB.
--- Benchmark hpgsql Tuple List (100000 rows): Wall time=3.706 s, total Haskell (does not include C heap) memory allocated=7145.2 MB.
--- Benchmark hasql Tuple List (100000 rows): Wall time=7.598 s, total Haskell (does not include C heap) memory allocated=4920.4 MB.
--- Benchmark postgresql-simple Tuple List (100000 rows): Wall time=12.62 s, total Haskell (does not include C heap) memory allocated=21619.9 MB.
--- Benchmark hpgsql Record List (100000 rows): Wall time=3.221 s, total Haskell (does not include C heap) memory allocated=7449.9 MB.
--- Benchmark hasql Record List (100000 rows): Wall time=6.345 s, total Haskell (does not include C heap) memory allocated=4904.8 MB.
--- Benchmark postgresql-simple Record List (100000 rows): Wall time=11.28 s, total Haskell (does not include C heap) memory allocated=20568.1 MB.
--- Benchmark hpgsql Tuple Stream (10000 rows): Wall time=160.5 ms, total Haskell (does not include C heap) memory allocated=653.4 MB.
--- Benchmark hasql Tuple Stream (10000 rows): Wall time=495.2 ms, total Haskell (does not include C heap) memory allocated=433.3 MB.
--- Benchmark streaming-postgresql-simple Tuple Stream (10000 rows): Wall time=1.396 s, total Haskell (does not include C heap) memory allocated=2778.9 MB.
--- Benchmark hpgsql Record Stream (10000 rows): Wall time=199.1 ms, total Haskell (does not include C heap) memory allocated=719.5 MB.
--- Benchmark hasql Record Stream (10000 rows): Wall time=530.8 ms, total Haskell (does not include C heap) memory allocated=474.5 MB.
--- Benchmark streaming-postgresql-simple Record Stream (10000 rows): Wall time=1.371 s, total Haskell (does not include C heap) memory allocated=2675.2 MB.
--- Benchmark hpgsql Tuple Stream (100000 rows): Wall time=1.371 s, total Haskell (does not include C heap) memory allocated=6518.0 MB.
--- Benchmark hasql Tuple Stream (100000 rows): Wall time=4.914 s, total Haskell (does not include C heap) memory allocated=4340.1 MB.
--- Benchmark streaming-postgresql-simple Tuple Stream (100000 rows): Wall time=14.20 s, total Haskell (does not include C heap) memory allocated=27952.5 MB.
--- Benchmark hpgsql Record Stream (100000 rows): Wall time=1.662 s, total Haskell (does not include C heap) memory allocated=7188.4 MB.
--- Benchmark hasql Record Stream (100000 rows): Wall time=5.139 s, total Haskell (does not include C heap) memory allocated=4752.1 MB.
--- Benchmark streaming-postgresql-simple Record Stream (100000 rows): Wall time=13.81 s, total Haskell (does not include C heap) memory allocated=26914.9 MB.
--- Benchmark hpgsql copyFromS binary COPY (100000 rows): Wall time=1.245 s, total Haskell (does not include C heap) memory allocated=11680.8 MB.
--- Benchmark postgresql-simple text COPY (100000 rows): Wall time=1.337 s, total Haskell (does not include C heap) memory allocated=4779.2MB.
```
