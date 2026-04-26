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
```csv
postgresql-simple Record List (100000 rows),12.1358,143.08M,91.2
hasql Record List (100000 rows),6.48715,142.48M,75.0
hpgsql Record List (100000 rows),4.51722,72.07M,82.1
```

### Materializing 100_000 rows with 13 columns each into a List of Tuples

This runs with 2 concurrent queries, 10 times over:
```csv
postgresql-simple Tuple List (100000 rows),14.5882,143.05M,139.1
hasql Tuple List (100000 rows),8.87797,142.48M,197.7
hpgsql Tuple List (100000 rows),5.17709,72.07M,132.6
```

### Streaming 100_000 rows with 13 columns as Records

This runs with 2 concurrent queries, 10 times over.
Hpgsql's implementation streams directly from the socket while the others use cursors, so
it might not be a fair comparison in terms of implementation (e.g. you can advance multiple
cursors simultaneously, but not hpgsql's Streamed-from-socket streams).
```csv
streaming-postgresql-simple Record Stream (100000 rows),13.9742,73.57M,0.1
postgresql-simple Record fold (100000 rows),13.5506,77.59M,0.1
hpgsql Record Stream (100000 rows),1.71435,72.07M,0.1
```

### Streaming 100_000 rows with 13 columns as Tuples

This runs with 2 concurrent queries, 10 times over.
Hpgsql's implementation streams directly from the socket while the others use cursors, so
it might not be a fair comparison in terms of implementation (e.g. you can advance multiple
cursors simultaneously, but not hpgsql's Streamed-from-socket streams).
```csv
streaming-postgresql-simple Tuple Stream (100000 rows),14.3788,73.27M,0.1
postgresql-simple Tuple fold (100000 rows),13.6508,82.00M,0.1
hpgsql Tuple Stream (100000 rows),1.2974,72.07M,0.1
```

### COPY FROM STDIN

This compares hpgsql's binary copy to a `forM` loop writing text rows.

```csv
postgresql-simple text COPY (100000 rows),1.46025,72.10M,3.9
hpgsql copyFromS binary COPY (100000 rows),1.26732,72.07M,10.9
```
