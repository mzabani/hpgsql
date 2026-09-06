# Benchmarks preface
hpgsql's repository contains a small benchmark suite that compares hpgsql to postgresql-simple, hasql, and streaming-postgresql-simple. It also compares hpgsql's streaming benchmark against equivalent standalone benchmarks written in Rust (tokio-postgres) and C# (Npgsql).

# Methodology for measuring peak memory usage

Wall clock time shown in these benchmarks should be reliable as we force garbage collection inside each benchmark.

Peak memory usage, however, is far trickier to measure. We use heaptrack to intercept and account for libc's allocation primitives, but the GHC runtime does not use `malloc` for its regular allocations, but does use it for ~72MB of allocations at application startup (on my machine). Some of the libraries we compare to also use libpq, which uses `malloc`.
So we use `peak_live_rts_memory + heaptrack_peak_memory - heaptrack_peak_memory_after_app_init` as the measure of peak memory for a given benchmark. This discards the initial memory allocated by the RTS regardless of what the app does, ignores extra memory used by the GC during copying (which I consider just a byproduct of memory allocation) but still accounts for memory allocated by the RTS and by libpq.

The downside of this approach is that peak memories as measured by both the managed runtime and heaptrack may be collected at different points in time, so we're effectively measuring an upper bound on peak memory allocated.

This is unfair towards libpq-based libraries in the comparison because hpgsql has no libc allocations at all, so it's precise for hpgsql, but an upper bound for the others. To somewhat counter that, we also measure peak live managed-runtime allocated memory independently, and total managed-runtime allocated memory as an even more distant proxy. Together, these can help us debug whether the upper bounds might be too far off.

The Rust (tokio-postgres) benchmark's `peak_memory_upper_bound` is measured only by heaptrack, making it a precise peak, not an upper bound.

> [!WARNING]
> I'm no expert using some of these libraries, so be careful interpreting results. I also welcome scrutiny and contributions.
> Noteworthy:
> - hpgsql's advantage wanes if the same benchmarks run with a higher number of concurrent connections/queries. This might indicate the bottleneck moves to networking and/or postgres, but it might indicate hpgsql gets worse comparatively with more connections. I don't know which it is.
> - These benchmarks were taken on non-encrypted TCP socket postgres connections, with Linux amd64, 32GB RAM.
> - Haskell libraries and the executable are built with -O1, not -O2.
> - Haskell records only have strict fields.

## Running benchmarks yourself
You can clone hpgsql and if you have Nix and direnv allowed, you should be able to run this with `run benchmarks` in the repository's root.

After that, list the contents of the `benchmark-results` folder and check the markdown (`.md`) table files. You can also use `heaptrack` to analyze the memory profiles collected there.

## Versions of libraries used here

- postgresql-libpq 0.11.0.0
- postgresql-simple 0.7.0.1
- hasql 1.9.3.1
- For Rust, tokio-postgres 0.7
- For C#, Npgsql 8.0.9

## Benchmarks' output on my computer

Results are written as markdown tables (one per `benchmark-results/*.md` file). The second column is wall clock time, the third is peak live RTS/Haskell memory, the fourth is the peak memory upper bound described above, and the fifth is total Haskell memory allocated over the whole benchmark.

### Streaming 100_000 rows with 17 columns as Records

This runs with 2 concurrent queries, 10 times over.

This benchmark is unfair towards both hpgsql and postgresql-simple because the row decoder is Generically derived for them while it is hand-written for hasql. The C# row is decoded field-by-field from a raw `NpgsqlDataReader` (no ORM/reflection involved), comparable in spirit to hasql's and rust-bench's hand-written decoders.

However, Hpgsql's implementation streams directly from the socket while the others use cursors, so
it might not be a fair comparison in terms of implementation (e.g. you can advance multiple
cursors simultaneously, but not hpgsql's Streamed-from-socket streams).

| name | wall_clock_time | peak_live_rts_memory | peak_memory_upper_bound | total_haskell_memory_allocated |
|---|---|---|---|---|
| streaming-postgresql-simple Record Stream (100000 rows, Generically derived row decoder) | 16.37s | 0.0MB | 1.2MB | 62278.5MB |
| postgresql-simple Record fold (100000 rows, Generically derived row decoder) | 16.13s | 0.0MB | 41.4MB | 48920.3MB |
| hpgsql Record Stream (100000 rows, Generically derived row decoder) | 1.627s | 0.3MB | 0.3MB | 14318.9MB |
| rust-tokio-postgres Record Stream (100000 rows) | 894.623086ms | - | 0.4MB | - |
| Npgsql Record Stream (100000 rows) | 1.043s | 0.2MB | 0.7MB | 441.3MB |

### Streaming 100_000 rows with 13 columns as Tuples

This runs with 2 concurrent queries, 10 times over.
Hpgsql's implementation streams directly from the socket while the others use cursors, so
it might not be a fair comparison in terms of implementation (e.g. you can advance multiple
cursors simultaneously, but not hpgsql's Streamed-from-socket streams).

| name | wall_clock_time | peak_live_rts_memory | peak_memory_upper_bound | total_haskell_memory_allocated |
|---|---|---|---|---|
| streaming-postgresql-simple Tuple Stream (100000 rows) | 13.27s | 0.0MB | 1.1MB | 55905.7MB |
| postgresql-simple Tuple fold (100000 rows) | 13.27s | 0.0MB | 11.8MB | 42538.0MB |
| hpgsql Tuple Stream (100000 rows) | 779.3ms | 0.2MB | 0.2MB | 8266.8MB |

### Materializing 100_000 rows with 17 columns each into a List of Records

This runs with 2 concurrent queries, 10 times over:

This benchmark is unfair towards both hpgsql and postgresql-simple because the row decoder is Generically derived for them while it is hand-written for hasql.

| name | wall_clock_time | peak_live_rts_memory | peak_memory_upper_bound | total_haskell_memory_allocated |
|---|---|---|---|---|
| postgresql-simple Record List (100000 rows, Generically derived row decoder) | 14.37s | 141.0MB | 225.1MB | 49595.8MB |
| hasql Record List (100000 rows) | 9.272s | 193.6MB | 379.9MB | 14422.7MB |
| hpgsql Record List (100000 rows, Generically derived row decoder) | 4.059s | 130.0MB | 130.0MB | 14966.5MB |

### Materializing 100_000 rows with 13 columns each into a List of Tuples

This runs with 2 concurrent queries, 10 times over:

| name | wall_clock_time | peak_live_rts_memory | peak_memory_upper_bound | total_haskell_memory_allocated |
|---|---|---|---|---|
| postgresql-simple Tuple List (100000 rows) | 14.10s | 148.4MB | 218.8MB | 43243.6MB |
| hasql Tuple List (100000 rows) | 7.618s | 200.8MB | 341.6MB | 9841.6MB |
| hpgsql Tuple List (100000 rows) | 4.060s | 147.8MB | 147.8MB | 9983.5MB |

### COPY FROM STDIN

This compares hpgsql's binary copy to a `forM` loop writing text rows.

| name | wall_clock_time | peak_live_rts_memory | peak_memory_upper_bound | total_haskell_memory_allocated |
|---|---|---|---|---|
| postgresql-simple text COPY (100000 rows) | 1.354s | 2.1MB | 2.1MB | 4779.0MB |
| hpgsql copyFromS binary COPY (100000 rows) | 966.3ms | 9.3MB | 9.3MB | 2358.2MB |
