#!/usr/bin/env bash

TABLE_HEADER="| name | wall_clock_time | peak_live_rts_memory | peak_memory_upper_bound | total_managed_memory_allocated |"
TABLE_SEPARATOR="|---|---|---|---|---|"

# Converts a fused "<number><unit>" wall-clock string (e.g. "14.69s",
# "895.5ms", "0.2μs") into milliseconds, so rows can be sorted slowest-to-
# fastest regardless of which unit each language picked for that row.
time_to_ms() {
  local val="$1"
  awk -v v="$val" 'BEGIN {
    n = v + 0
    if (v ~ /ms$/) printf "%.6f", n
    else if (v ~ /(μs|us)$/) printf "%.6f", n / 1000
    else if (v ~ /ns$/) printf "%.6f", n / 1000000
    else if (v ~ /s$/) printf "%.6f", n * 1000
    else printf "%.6f", 0
  }'
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

# Renders an MB value for a table cell, or "-" when the value is absent (the
# benchmark's own stdout didn't report that metric at all, or its heaptrack
# peak was skipped -- see run_bench below).
fmt_mb() {
  if [ -n "$1" ]; then echo "${1}MB"; else echo "-"; fi
}

# Runs one benchmark and appends a row to a markdown table. All three
# languages' benchmark executables print output in a shared shape --
# "Wall time=<value> <unit>," and (Haskell only) "memory allocated=<value>
# MB." -- so the same greps work across all of them, and a metric a given
# language doesn't report simply comes up empty (rendered as "-").
#
# `floor` controls whether/how a heaptrack-based peak_memory_upper_bound is
# computed (see "Methodology for measuring peak memory usage" in
# BENCHMARKS.md):
#   - a number: subtract this app-init floor from heaptrack's peak, then add
#     back peak_live_rts_memory (Haskell's upper-bound estimate, since its GC
#     heap is invisible to heaptrack).
#   - "0": no floor to subtract, just report heaptrack's raw peak (Rust,
#     whose default allocator is malloc-backed so heaptrack sees ~all of it).
#   - "" (empty): skip heaptrack entirely, leave peak_memory_upper_bound as
#     "-" (C#, whose GC.GetTotalMemory can't be trusted as a true peak the
#     way GHC's max_live_bytes can -- see BENCHMARKS.md).
run_bench() {
  local table_file="$1"
  local bench_name="$2"
  local floor="$3"
  shift 3
  local cmd=("$@")
  echo "$bench_name"

  BENCH_OUTPUT=$("${cmd[@]}" 2>&1)
  # Each language picks whichever time unit fits (s/ms/μs/...), with a space
  # between the number and the unit -- keep the unit, drop the space.
  WALLCLOCK_TIME=$(echo "$BENCH_OUTPUT" | grep -oP '(?<=Wall time=)[0-9.]+ \S+(?=,)' | tr -d ' ')
  PEAK_LIVE_MB=$(echo "$BENCH_OUTPUT" | grep -oP '(?<=--- Peak live data \(max_live_bytes\): )\S+')
  TOTAL_ALLOCATED_MB=$(echo "$BENCH_OUTPUT" | grep -oP '(?<=memory allocated=)\S+(?= MB\.)')

  local peak_upper_mb=""
  if [ -n "$floor" ]; then
    heaptrack --record-only -o "benchmark-results/heaptrack.outdat" "${cmd[@]}" 2>/dev/null
    local peakheap
    peakheap=$(heaptrack_print -f "benchmark-results/heaptrack.outdat.zst" | grep "peak heap memory consumption:" | awk -F': ' '{print $2}')
    mv "benchmark-results/heaptrack.outdat.zst" "benchmark-results/$bench_name.outdat.zst"
    peak_upper_mb=$(awk -v live="${PEAK_LIVE_MB:-0}" -v heap="$(to_mb "$peakheap")" -v floor="$floor" 'BEGIN{printf "%.1f", live + heap - floor}')
  fi

  # Italicize hpgsql's own row so it stands out against the libraries it's
  # compared to.
  local display_name="$bench_name"
  if [[ "$bench_name" == hpgsql* ]]; then
    display_name="*$bench_name*"
  fi

  # Rows are staged with a sortable millisecond key (stripped off below by
  # finalize_tables) rather than written straight to the table file, so the
  # whole table can be sorted slowest-to-fastest once every contributing
  # run_bench call (possibly across several, e.g. Haskell + Rust + C# all
  # writing to record_stream_bench.md) has finished.
  local row="| $display_name | ${WALLCLOCK_TIME:--} | $(fmt_mb "$PEAK_LIVE_MB") | $(fmt_mb "$peak_upper_mb") | $(fmt_mb "$TOTAL_ALLOCATED_MB") |"
  echo -e "$(time_to_ms "$WALLCLOCK_TIME")\t$row" >> "benchmark-results/.rows-$table_file"
}

# Writes every staged table (see run_bench above) as a final markdown file,
# sorted slowest-to-fastest by wall-clock time.
finalize_tables() {
  local rows_file
  for rows_file in benchmark-results/.rows-*; do
    [ -e "$rows_file" ] || continue
    local table_file="${rows_file#benchmark-results/.rows-}"
    {
      echo "$TABLE_HEADER"
      echo "$TABLE_SEPARATOR"
      sort -t $'\t' -k1,1 -rn "$rows_file" | cut -f2-
    } > "benchmark-results/$table_file"
    rm "$rows_file"
  done
}

# Runs run_bench once per name in a group, all against the same executable
# with the same heaptrack floor -- for the Haskell benchmark executable's
# --match flag, which selects one benchmark per invocation.
run_bench_group() {
  local table_file="$1"
  local floor="$2"
  local exe="$3"
  shift 3
  local names=("$@")
  for b in "${names[@]}"; do
    run_bench "$table_file" "$b" "$floor" "$exe" --match "$b"
  done
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
echo "App-init heap memory floor (subtracted from every Haskell benchmark's heaptrack peak below): ${APP_INIT_PEAK_MB} M"

run_bench_group record_list_bench.md "$APP_INIT_PEAK_MB" "$benchexe" "${record_list_bench[@]}"
run_bench_group tuple_list_bench.md "$APP_INIT_PEAK_MB" "$benchexe" "${tuple_list_bench[@]}"
run_bench_group record_stream_bench.md "$APP_INIT_PEAK_MB" "$benchexe" "${record_stream_bench[@]}"
run_bench_group tuple_stream_bench.md "$APP_INIT_PEAK_MB" "$benchexe" "${tuple_stream_bench[@]}"
run_bench_group copy_bench.md "$APP_INIT_PEAK_MB" "$benchexe" "${copy_bench[@]}"

# rust-bench mirrors hpgsql's Record Stream benchmark (streamed, generically
# decoded rows, discarded as they arrive), so its result is recorded alongside
# it. floor="0": no app-init floor to subtract, just heaptrack's raw peak,
# since Rust's default allocator is malloc-backed and heaptrack sees ~all of
# its heap (see BENCHMARKS.md).
run_bench record_stream_bench.md "rust-tokio-postgres Record Stream (100000 rows)" "0" "$rust_benchexe"

# The Npgsql benchmark also mirrors hpgsql's Record Stream benchmark (same 17
# columns, streamed and discarded as they arrive), so it joins the same
# table. floor="": heaptrack is skipped entirely, leaving both memory columns
# as "-" (see BENCHMARKS.md).
run_bench record_stream_bench.md "Npgsql Record Stream (100000 rows)" "" "${csharp_benchexe[@]}"

finalize_tables
