# Benchmarks preface
hpgsql's repository contains a small benchmark suite that compares hpgsql to postgresql-simple, hasql, and streaming-postgresql-simple.

# Methodology for measuring peak memory usage

Wall clock time shown in these benchmarks should be reliable as we force garbage collection inside each benchmark.

Peak memory usage, however, is far trickier to measure. We use heaptrack to intercept and account for libc's allocation primitives, but the GHC runtime does not use `malloc` for its regular allocations, but does use it for ~72MB of allocations at application startup. Some of the libraries we compare to also use libpq, which uses `malloc` under the hood.
So we use `peak_live_rts_memory + heaptrack_peak_memory - heaptrack_peak_memory_after_app_init` as the measure of peak memory for a given benchmark. This discards the initial memory allocated by the RTS regardless of what the app does, ignores extra memory used by the GC during copying (which I consider just a byproduct of memory allocation) but still accounts for memory allocated by the RTS and by libpq.

The downside of this approach is that peak memories as measured by both the GHC runtime and heaptrack may be collected at different points in time, so we're effectively measuring an upper bound on peak memory allocated.

This is unfair towards libpq-based libraries in the comparison because hpgsql has no libc allocations at all, so it's precise for hpgsql, but an upper bound for the others. To somewhat counter that, we also measure peak live Haskell allocated memory independently, and total Haskell allocated memory as an even more distant proxy. Together, these can help us debug whether the upper bounds might be too far off.

The Rust (tokio-postgres) benchmark's `peak_memory_upper_bound` is measured only by heaptrack, making it a precise peak, not an upper bound.

> [!WARNING]
> I'm no expert using some of these libraries, so be careful interpreting results. I also welcome scrutiny and contributions.
> Noteworthy:
> - hpgsql's advantage wanes if the same benchmarks run with a higher number of concurrent connections/queries. This might indicate the bottleneck moves to networking and/or postgres, but it might indicate hpgsql gets worse comparatively with more connections. I don't know which it is.
> - These benchmarks were taken on non-encrypted Unix Domain socket postgres connections, with Linux amd64, 32GB RAM.

## Running benchmarks yourself
You can clone hpgsql and if you have Nix and direnv allowed, you should be able to run this with `run benchmarks` in the repository's root.

After that, list the contents of the `benchmark-results` folder and check the markdown (`.md`) table files. You can also use `heaptrack` to analyze the memory profiles collected there.

## Versions of libraries used here

- postgresql-libpq 0.11.0.0
- postgresql-simple 0.7.0.1
- hasql 1.9.3.1

## Benchmarks' output on my computer

Results are written as markdown tables (one per `benchmark-results/*.md` file). The second column is wall clock time, the third is peak live RTS/Haskell memory, the fourth is the peak memory upper bound described above, and the fifth is total Haskell memory allocated over the whole benchmark.

### Materializing 100_000 rows with 17 columns each into a List of Records

This runs with 2 concurrent queries, 10 times over:

This benchmark is unfair towards both hpgsql and postgresql-simple because the row decoder is Generically derived for them while it is hand-written for hasql.

| name | wall_clock_time | peak_live_rts_memory | peak_memory_upper_bound | total_haskell_memory_allocated |
|---|---|---|---|---|
| postgresql-simple Record List (100000 rows, Generically derived row decoder) | 14.84s | 138.6MB | 306.8MB | 49595.1MB |
| hasql Record List (100000 rows) | 8.659s | 180.2MB | 366.5MB | 14422.7MB |
| hpgsql Record List (100000 rows, Generically derived row decoder) | 4.034s | 140.1MB | 140.1MB | 14949.0MB |

### Materializing 100_000 rows with 13 columns each into a List of Tuples

This runs with 2 concurrent queries, 10 times over:

| name | wall_clock_time | peak_live_rts_memory | peak_memory_upper_bound | total_haskell_memory_allocated |
|---|---|---|---|---|
| postgresql-simple Tuple List (100000 rows) | 13.90s | 147.3MB | 218.3MB | 43242.4MB |
| hasql Tuple List (100000 rows) | 7.817s | 196.1MB | 336.9MB | 9841.6MB |
| hpgsql Tuple List (100000 rows) | 3.842s | 150.8MB | 150.8MB | 9979.0MB |

### Streaming 100_000 rows with 17 columns as Records

This runs with 2 concurrent queries, 10 times over.

This benchmark is unfair towards both hpgsql and postgresql-simple because the row decoder is Generically derived for them while it is hand-written for hasql.

However, Hpgsql's implementation streams directly from the socket while the others use cursors, so
it might not be a fair comparison in terms of implementation (e.g. you can advance multiple
cursors simultaneously, but not hpgsql's Streamed-from-socket streams).

| name | wall_clock_time | peak_live_rts_memory | peak_memory_upper_bound | total_haskell_memory_allocated |
|---|---|---|---|---|
| streaming-postgresql-simple Record Stream (100000 rows, Generically derived row decoder) | 16.26s | 0.0MB | 1.1MB | 62278.4MB |
| postgresql-simple Record fold (100000 rows, Generically derived row decoder) | 15.98s | 0.0MB | 34.5MB | 48918.8MB |
| hpgsql Record Stream (100000 rows, Generically derived row decoder) | 1.573s | 0.3MB | 0.3MB | 14304.0MB |
| rust-tokio-postgres Record Stream (100000 rows) | 793.522778ms | - | 0.4MB | - |

### Streaming 100_000 rows with 13 columns as Tuples

This runs with 2 concurrent queries, 10 times over.
Hpgsql's implementation streams directly from the socket while the others use cursors, so
it might not be a fair comparison in terms of implementation (e.g. you can advance multiple
cursors simultaneously, but not hpgsql's Streamed-from-socket streams).

| name | wall_clock_time | peak_live_rts_memory | peak_memory_upper_bound | total_haskell_memory_allocated |
|---|---|---|---|---|
| streaming-postgresql-simple Tuple Stream (100000 rows) | 13.42s | 0.0MB | 1.4MB | 55905.6MB |
| postgresql-simple Tuple fold (100000 rows) | 12.69s | 0.0MB | 12.6MB | 42536.5MB |
| hpgsql Tuple Stream (100000 rows) | 770.8ms | 0.3MB | 0.3MB | 8264.5MB |

### COPY FROM STDIN

This compares hpgsql's binary copy to a `forM` loop writing text rows.

| name | wall_clock_time | peak_live_rts_memory | peak_memory_upper_bound | total_haskell_memory_allocated |
|---|---|---|---|---|
| postgresql-simple text COPY (100000 rows) | 1.296s | 2.1MB | 2.1MB | 4779.2MB |
| hpgsql copyFromS binary COPY (100000 rows) | 561.8ms | 10.0MB | 10.0MB | 2358.2MB |
