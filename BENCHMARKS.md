# Benchmarks preface
hpgsql's repository contains a small benchmark suite that compares hpgsql to postgresql-simple, hasql, and streaming-postgresql-simple.

> [!WARNING]
> I'm no expert using some of these libraries, so be careful interpreting results. I also welcome scrutiny and contributions.
> Noteworthy:
> - While wall clock times are probably reliable, peak heap allocations are measured by [heaptrack](https://github.com/KDE/heaptrack), and I'm no specialist in using it, nor do I understand glibc's and GHC RTS's allocation mechanisms well enough to know if I'm measuring something meaningful.
>   - In fact, I cannot explain why peak live memory as reported by GHC's `getRTSStats` is some times higher than total memory reported by heaptrack.
> - hpgsql's advantage wanes if the same benchmarks run with a higher number of concurrent connections/queries. This might indicate the bottleneck moves to networking and/or postgres, but it might indicate hpgsql gets worse comparatively with more connections. I don't know which it is.
> - On my computer, GHC's runtime seems to allocate ~72MB in the heap even when no work is done, so it's not obvious what memory allocated per row (or some unit of work) is for each library.
> - These benchmarks were taken on non-encrypted Unix Domain socket postgres connections, with Linux amd64, 32GB RAM.

## Running benchmarks yourself
You can clone hpgsql and if you have Nix and direnv allowed, you should be able to run this with `run benchmarks` in the repository's root.

After that, list the contents of the `benchmark-results` folder and check the csv files. You can also use `heaptrack` to analyze the memory profiles collected there.

## Versions of libraries used here

- postgresql-libpq 0.11.0.0
- postgresql-simple 0.7.0.1
- hasql 1.9.3.1

## Benchmarks' output on my computer

The second column is wall clock time in seconds, the third is peak heap memory allocated as measured by heaptrack (so big caveats like I explained up there), the fourth is peak live RTS memory as per getRTSStats.

### Materializing 100_000 rows with 13 columns each into a List of Records

This runs with 2 concurrent queries, 10 times over:

This benchmark is unfair towards both hpgsql and postgresql-simple because the row decoder is Generically derived for them while it is hand-written for hasql.
```csv
postgresql-simple Record List (100000 rows),11.99,142.52M,93.7
hasql Record List (100000 rows),6.290,142.48M,74.7
hpgsql Record List (100000 rows),4.118,72.07M,119.8
```

### Materializing 100_000 rows with 13 columns each into a List of Tuples

This runs with 2 concurrent queries, 10 times over:
```csv
postgresql-simple Tuple List (100000 rows),14.79,142.52M,140.1
hasql Tuple List (100000 rows),8.410,142.48M,203.2
hpgsql Tuple List (100000 rows),4.619,72.07M,150.5
```

### Streaming 100_000 rows with 13 columns as Records

This runs with 2 concurrent queries, 10 times over.

This benchmark is unfair towards both hpgsql and postgresql-simple because the row decoder is Generically derived for them while it is hand-written for hasql.

However, Hpgsql's implementation streams directly from the socket while the others use cursors, so
it might not be a fair comparison in terms of implementation (e.g. you can advance multiple
cursors simultaneously, but not hpgsql's Streamed-from-socket streams).
```csv
streaming-postgresql-simple Record Stream (100000 rows),13.40,73.26M,0.0
postgresql-simple Record fold (100000 rows),13.40,78.20M,0.0
hpgsql Record Stream (100000 rows),1.427,72.07M,0.0
```

### Streaming 100_000 rows with 13 columns as Tuples

This runs with 2 concurrent queries, 10 times over.
Hpgsql's implementation streams directly from the socket while the others use cursors, so
it might not be a fair comparison in terms of implementation (e.g. you can advance multiple
cursors simultaneously, but not hpgsql's Streamed-from-socket streams).
```csv
streaming-postgresql-simple Tuple Stream (100000 rows),13.97,73.27M,0.0
postgresql-simple Tuple fold (100000 rows),13.55,84.22M,0.0
hpgsql Tuple Stream (100000 rows),1.032,72.07M,0.0
```

### COPY FROM STDIN

This compares hpgsql's binary copy to a `forM` loop writing text rows.

```csv
postgresql-simple text COPY (100000 rows),1.353,72.10M,3.8
hpgsql copyFromS binary COPY (100000 rows),682.4,72.07M,10.8
```
