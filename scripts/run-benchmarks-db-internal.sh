#!/usr/bin/env bash

time_seconds() {
  local start end
  start=$(date +%s.%N)
  "$@" >&2
  end=$(date +%s.%N)
  awk "BEGIN {print $end - $start}"
}

run_bench() {
  local csv_file="$1"
  touch benchmark-results/"$csv_file"
  shift
  local bench_names=("$@")
  for b in "${bench_names[@]}"; do
    echo "$b"
    # Measure wall-clock time without heaptrack to avoid interference
    # Capture stdout to extract RTS peak live data
    local start end
    start=$(date +%s.%N)
    BENCH_OUTPUT=$("$benchexe" --match "$b" 2>&1)
    end=$(date +%s.%N)
    WALLCLOCK_TIME=$(awk "BEGIN {print $end - $start}")
    PEAK_LIVE_MB=$(echo "$BENCH_OUTPUT" | grep -oP '(?<=--- Peak live data \(max_live_bytes\): )\S+')
    heaptrack --record-only -o "benchmark-results/heaptrack.outdat" "$benchexe" --match "$b" 2>/dev/null
    PEAKHEAP=$(heaptrack_print -f "benchmark-results/heaptrack.outdat.zst" | grep "peak heap memory consumption:" | awk -F': ' '{print $2}')
    mv "benchmark-results/heaptrack.outdat.zst" "benchmark-results/$b.outdat.zst"
    echo "$b,$WALLCLOCK_TIME,$PEAKHEAP,$PEAK_LIVE_MB" >> "benchmark-results/$csv_file"
  done
}

record_list_bench=("postgresql-simple Record List (100000 rows)" "hasql Record List (100000 rows)" "hpgsql Record List (100000 rows)")
tuple_list_bench=("postgresql-simple Tuple List (100000 rows)" "hasql Tuple List (100000 rows)" "hpgsql Tuple List (100000 rows)")
record_stream_bench=("streaming-postgresql-simple Record Stream (100000 rows)" "postgresql-simple Record fold (100000 rows)" "hpgsql Record Stream (100000 rows)")
tuple_stream_bench=("streaming-postgresql-simple Tuple Stream (100000 rows)" "postgresql-simple Tuple fold (100000 rows)" "hpgsql Tuple Stream (100000 rows)")
copy_bench=("postgresql-simple text COPY (100000 rows)" "hpgsql copyFromS binary COPY (100000 rows)")

# Wipe the folder, recreate it and run the benchmarks
rm benchmark-results -rf
mkdir benchmark-results
benchexe=$(cabal list-bin -O1 hpgsql-benchmarks)

run_bench record_list_bench.csv "${record_list_bench[@]}"
run_bench tuple_list_bench.csv "${tuple_list_bench[@]}"
run_bench record_stream_bench.csv "${record_stream_bench[@]}"
run_bench tuple_stream_bench.csv "${tuple_stream_bench[@]}"
run_bench copy_bench.csv "${copy_bench[@]}"

