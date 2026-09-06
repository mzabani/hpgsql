#!/usr/bin/env bash

TABLE_HEADER="| name | wall_clock_time | peak_live_rts_memory | peak_memory_upper_bound | total_managed_memory_allocated |"
TABLE_SEPARATOR="|---|---|---|---|---|"

# Creates the markdown table file with a header if it doesn't already exist,
# so that calling this for a file shared by multiple run_bench/run_rust_bench
# calls (e.g. record_stream_bench.md) doesn't duplicate the header.
init_table() {
  local table_file="$1"
  local path="benchmark-results/$table_file"
  if [ ! -f "$path" ]; then
    echo "$TABLE_HEADER" > "$path"
    echo "$TABLE_SEPARATOR" >> "$path"
  fi
}

# Converts a heaptrack-formatted size string (e.g. "72.07M", "1.2G", "512K",
# "0B") to a plain number of megabytes, so it can be combined arithmetically
# with peak_live_rts_memory (already reported in MB by the benchmark
# executable itself).
to_mb() {
  local val="$1"
  awk -v v="$val" 'BEGIN {
    unit = substr(v, length(v), 1)
    if (unit ~ /[A-Za-z]/) {
      num = substr(v, 1, length(v) - 1) + 0
    } else {
      unit = "B"
      num = v + 0
    }
    if (unit == "G") printf "%.4f", num * 1024
    else if (unit == "K") printf "%.4f", num / 1024
    else if (unit == "B") printf "%.4f", num / 1024 / 1024
    else printf "%.4f", num
  }'
}

run_bench() {
  local table_file="$1"
  init_table "$table_file"
  shift
  local bench_names=("$@")
  for b in "${bench_names[@]}"; do
    echo "$b"
    # Measure wall-clock time without heaptrack to avoid interference
    # Capture stdout to extract RTS peak live data
    BENCH_OUTPUT=$("$benchexe" --match "$b" 2>&1)
    # criterion's `secs` picks whichever unit fits (s/ms/μs/...), with a space
    # between the number and the unit -- keep the unit, drop the space.
    WALLCLOCK_TIME=$(echo "$BENCH_OUTPUT" | grep -oP '(?<=Wall time=)[0-9.]+ \S+(?=,)' | tr -d ' ')
    PEAK_LIVE_MB=$(echo "$BENCH_OUTPUT" | grep -oP '(?<=--- Peak live data \(max_live_bytes\): )\S+')
    # Sanity-check signal alongside peak_memory_upper_bound_mb (see
    # BENCHMARKS.md): cumulative bytes allocated over the whole run, as
    # opposed to a peak. Only available on the Haskell side (GHC.Stats), since
    # heaptrack has no comparable "total ever allocated" figure to report.
    TOTAL_ALLOCATED_MB=$(echo "$BENCH_OUTPUT" | grep -oP '(?<=memory allocated=)\S+(?= MB\.)')

    # Now run with heaptrack to track peak heap memory usage
    heaptrack --record-only -o "benchmark-results/heaptrack.outdat" "$benchexe" --match "$b" 2>/dev/null
    PEAKHEAP=$(heaptrack_print -f "benchmark-results/heaptrack.outdat.zst" | grep "peak heap memory consumption:" | awk -F': ' '{print $2}')
    mv "benchmark-results/heaptrack.outdat.zst" "benchmark-results/$b.outdat.zst"

    # See "Methodology for measuring peak memory usage" in BENCHMARKS.md: this
    # discards the app-init floor (mostly GHC's per-capability eventlog
    # buffers, which heaptrack does see since they're malloc'd) from
    # heaptrack's peak, then adds back the RTS's own peak live Haskell heap
    # (which heaptrack can't see, since it's mmap'd megablocks, not malloc).
    # It's an upper bound, not an exact simultaneous peak, since the two peaks
    # may occur at different times.
    ESTIMATED_PEAK_MB=$(awk -v live="$PEAK_LIVE_MB" -v heap="$(to_mb "$PEAKHEAP")" -v floor="$APP_INIT_PEAK_MB" 'BEGIN{printf "%.1f", live + heap - floor}')

    echo "| $b | $WALLCLOCK_TIME | ${PEAK_LIVE_MB}MB | ${ESTIMATED_PEAK_MB}MB | ${TOTAL_ALLOCATED_MB}MB |" >> "benchmark-results/$table_file"
  done
}

# Like run_bench, but for the C# (Npgsql) benchmark executable, which has no
# --match flag (it only runs the one benchmark it was built with). Its two
# peak-memory columns are left as "-": .NET's GC.GetTotalMemory is a snapshot,
# not a tracked running maximum like GHC's max_live_bytes, so it can't be
# relied on the same way, and there's no floor-subtracted heaptrack estimate
# to fall back on either. Only wall-clock time and total allocated bytes are
# reported.
run_csharp_bench() {
  local table_file="$1"
  local bench_name="$2"
  init_table "$table_file"
  echo "$bench_name"

  BENCH_OUTPUT=$("${csharp_benchexe[@]}" 2>&1)
  WALLCLOCK_TIME=$(echo "$BENCH_OUTPUT" | grep -oP '(?<=Wall time=)[0-9.]+ \S+(?=,)' | tr -d ' ')
  TOTAL_ALLOCATED_MB=$(echo "$BENCH_OUTPUT" | grep -oP '(?<=memory allocated=)\S+(?= MB\.)')

  echo "| $bench_name | $WALLCLOCK_TIME | - | - | ${TOTAL_ALLOCATED_MB}MB |" >> "benchmark-results/$table_file"
}

# Like run_bench, but for the Rust (tokio-postgres) benchmark executable, which
# has no --match flag (it only runs the one benchmark it was built with) and,
# having no GHC runtime, cannot report peak_live_rts_memory or
# total_managed_memory_allocated -- those are left as "-" for its row.
run_rust_bench() {
  local table_file="$1"
  local bench_name="$2"
  init_table "$table_file"
  echo "$bench_name"

  BENCH_OUTPUT=$("$rust_benchexe" 2>&1)
  # Rust's Duration Debug format has no space between the number and whichever
  # unit fits (ns/µs/ms/s), so a plain non-whitespace token captures both.
  WALLCLOCK_TIME=$(echo "$BENCH_OUTPUT" | sed -n 's/^Wall clock time[^:]*: \(\S\+\).*/\1/p')

  # Still record with heaptrack so the profile is available for manual
  # analysis, in addition to the peak reported below.
  heaptrack --record-only -o "benchmark-results/heaptrack.outdat" "$rust_benchexe" 2>/dev/null
  PEAKHEAP=$(heaptrack_print -f "benchmark-results/heaptrack.outdat.zst" | grep "peak heap memory consumption:" | awk -F': ' '{print $2}')
  mv "benchmark-results/heaptrack.outdat.zst" "benchmark-results/$bench_name.outdat.zst"

  # Unlike GHC, Rust's default allocator routes through malloc, so heaptrack
  # sees essentially all of its heap -- there's no GC-managed chunk hidden
  # from it the way there is for the Haskell benchmarks. So this is a precise
  # peak, not one term of an upper-bound estimate, and no floor subtraction
  # applies (that floor is specific to the Haskell binary's own GHC RTS and
  # linked C libraries, and has no meaning for this unrelated executable).
  ESTIMATED_PEAK_MB=$(awk -v v="$(to_mb "$PEAKHEAP")" 'BEGIN{printf "%.1f", v}')

  echo "| $bench_name | $WALLCLOCK_TIME | - | ${ESTIMATED_PEAK_MB}MB | - |" >> "benchmark-results/$table_file"
}

record_list_bench=("postgresql-simple Record List (100000 rows, Generically derived row decoder)" "hasql Record List (100000 rows)" "hpgsql Record List (100000 rows, Generically derived row decoder)")
tuple_list_bench=("postgresql-simple Tuple List (100000 rows)" "hasql Tuple List (100000 rows)" "hpgsql Tuple List (100000 rows)")
record_stream_bench=("streaming-postgresql-simple Record Stream (100000 rows, Generically derived row decoder)" "postgresql-simple Record fold (100000 rows, Generically derived row decoder)" "hpgsql Record Stream (100000 rows, Generically derived row decoder)")
tuple_stream_bench=("streaming-postgresql-simple Tuple Stream (100000 rows)" "postgresql-simple Tuple fold (100000 rows)" "hpgsql Tuple Stream (100000 rows)")
copy_bench=("postgresql-simple text COPY (100000 rows)" "hpgsql copyFromS binary COPY (100000 rows)")

# Compile executables
cabal build hpgsql-benchmarks
cargo build --release --manifest-path rust-bench/Cargo.toml
dotnet build -c Release csharp-benchmarks/CsharpBenchmarks.csproj

# Wipe the folder, recreate it and run the benchmarks
rm benchmark-results -rf
mkdir benchmark-results
benchexe=$(cabal list-bin hpgsql-benchmarks)
rust_benchexe="./rust-bench/target/release/rust-bench"
# Invoked via `dotnet <dll>` rather than the native apphost binary directly:
# the apphost can't locate libhostfxr.so outside of a `dotnet run`/`dotnet
# exec` context (e.g. under nix-shell), and fails silently as far as this
# script is concerned (its stdout carries only an error message, so
# BENCH_OUTPUT's greps below all come up empty instead of erroring loudly).
csharp_benchexe=(dotnet "./csharp-benchmarks/bin/Release/net8.0/CsharpBenchmarks.dll")

# Measure the app-init memory floor once for the whole run: this matches no
# benchmark name, so hspec starts up the GHC RTS and immediately exits without
# running any benchmark's DB connections/queries. This floor is workload-
# independent (see BENCHMARKS.md), so collecting it once and reusing it for
# every benchmark below is both cheaper and more correct than remeasuring it
# per benchmark.
heaptrack --record-only -o "benchmark-results/heaptrack.outdat" "$benchexe" --match "no benchmark name matches this" 2>/dev/null
APP_INIT_PEAK_MB=$(to_mb "$(heaptrack_print -f "benchmark-results/heaptrack.outdat.zst" | grep "peak heap memory consumption:" | awk -F': ' '{print $2}')")
mv "benchmark-results/heaptrack.outdat.zst" "benchmark-results/app-init-baseline.outdat.zst"
echo "App-init heap memory floor (subtracted from every benchmark's heaptrack peak below): ${APP_INIT_PEAK_MB} M"

# Same idea as above, but for the C# binary's own CLR + Npgsql startup
# floor, which is unrelated to (and measured separately from) GHC's floor.
CSHARP_BENCH_FLOOR_ONLY=1 heaptrack --record-only -o "benchmark-results/heaptrack.outdat" "${csharp_benchexe[@]}" 2>/dev/null
CSHARP_APP_INIT_PEAK_MB=$(to_mb "$(heaptrack_print -f "benchmark-results/heaptrack.outdat.zst" | grep "peak heap memory consumption:" | awk -F': ' '{print $2}')")
mv "benchmark-results/heaptrack.outdat.zst" "benchmark-results/csharp-app-init-baseline.outdat.zst"
echo "C# app-init heap memory floor (subtracted from the C# benchmark's heaptrack peak below): ${CSHARP_APP_INIT_PEAK_MB} M"

run_bench record_list_bench.md "${record_list_bench[@]}"
run_bench tuple_list_bench.md "${tuple_list_bench[@]}"
run_bench record_stream_bench.md "${record_stream_bench[@]}"
run_bench tuple_stream_bench.md "${tuple_stream_bench[@]}"
run_bench copy_bench.md "${copy_bench[@]}"

# rust-bench mirrors hpgsql's Record Stream benchmark (streamed, generically
# decoded rows, discarded as they arrive), so its result is recorded alongside it.
run_rust_bench record_stream_bench.md "rust-tokio-postgres Record Stream (100000 rows)"

# The Npgsql benchmark also mirrors hpgsql's Record Stream benchmark (same 17
# columns, streamed and discarded as they arrive), so it joins the same table.
run_csharp_bench record_stream_bench.md "Npgsql Record Stream (100000 rows)"
