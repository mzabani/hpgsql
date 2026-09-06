#!/usr/bin/env nu

const TABLE_HEADER = "| name | wall_clock_time | peak_live_rts_memory | peak_memory_upper_bound | total_managed_memory_allocated |"
const TABLE_SEPARATOR = "|---|---|---|---|---|"

# Converts a heaptrack-formatted size string (e.g. "72.07M", "1.2G", "512K",
# "0B") to a plain number of megabytes, so it can be combined arithmetically
# with peak_live_rts_memory (already reported in MB by the benchmark
# executable itself).
def to-mb [val: string]: nothing -> float {
  let m = ($val | str trim | parse -r '^(?<num>[0-9.]+)(?<unit>[A-Za-z]?)$')
  if ($m | is-empty) {
    0.0
  } else {
    let num = ($m.0.num | into float)
    match $m.0.unit {
      "G" => ($num * 1024)
      "K" => ($num / 1024)
      "B" => ($num / 1024 / 1024)
      "" => ($num / 1024 / 1024)
      _ => $num # "M"
    }
  }
}

# Converts a fused "<number><unit>" wall-clock string (e.g. "14.69s",
# "895.5ms", "0.2μs") into milliseconds, so benchmark results can be sorted
# slowest-to-fastest regardless of which unit each language picked.
def time-to-ms [val: string]: nothing -> float {
  let m = ($val | parse -r '^(?<num>[0-9.]+)(?<unit>ms|μs|us|ns|s)$')
  if ($m | is-empty) {
    0.0
  } else {
    let num = ($m.0.num | into float)
    match $m.0.unit {
      "ms" => $num
      "μs" | "us" => ($num / 1000)
      "ns" => ($num / 1_000_000)
      "s" => ($num * 1000)
      _ => 0.0
    }
  }
}

# Renders an MB value for a table cell, or "-" when the value is absent (the
# benchmark's own stdout didn't report that metric at all, or its heaptrack
# peak was skipped -- see run-bench below).
def fmt-mb [val: any]: nothing -> string {
  if $val == null { "-" } else { $"($val)MB" }
}

# Extracts the "Wall time=<value> <unit>," fragment that all three languages'
# benchmark executables print in the same shape, and returns it fused (no
# space) e.g. "14.690s". Returns null if the benchmark's stdout has no such
# line.
def extract-wall-time [output: string]: nothing -> any {
  let m = ($output | parse -r 'Wall time=(?<num>[0-9.]+) (?<unit>\S+),')
  if ($m | is-empty) { null } else { $"($m.0.num)($m.0.unit)" }
}

# Extracts Haskell's "--- Peak live data (max_live_bytes): <value> M" line.
# Rust and C# never print this line at all, so this returns null for them --
# not a suppressed/discarded value, a genuine absence (see BENCHMARKS.md for
# why C#'s GC.GetTotalMemory isn't trustworthy enough to report here either).
def extract-peak-live-mb [output: string]: nothing -> any {
  let m = ($output | parse -r '--- Peak live data \(max_live_bytes\): (?<val>\S+)')
  if ($m | is-empty) { null } else { ($m.0.val | into float) }
}

# Extracts the "memory allocated=<value> MB." fragment shared by Haskell's
# and C#'s benchmark output (cumulative bytes allocated over the whole run,
# as opposed to a peak). Rust doesn't report this -- null for it.
def extract-total-allocated-mb [output: string]: nothing -> any {
  let m = ($output | parse -r 'memory allocated=(?<val>\S+) MB\.')
  if ($m | is-empty) { null } else { ($m.0.val | into float) }
}

# Reads heaptrack_print's "peak heap memory consumption: <value>" line out of
# a recording and converts it to MB.
def heaptrack-peak-mb [zst_path: string]: nothing -> float {
  let line = (^heaptrack_print -f $zst_path | lines | where ($it | str contains "peak heap memory consumption:") | first)
  to-mb ($line | split row ": " | last)
}

# One benchmark's result: the shape every run-bench call returns, regardless
# of which language it came from. peak_live_mb, peak_upper_mb and
# total_allocated_mb are all nullable -- a language that doesn't report a
# given metric simply has null there, rendered as "-" in the final table.
def new-benchmark-result [
  name: string
  wall_ms: float
  wall_display: any
  peak_live_mb: any
  peak_upper_mb: any
  total_allocated_mb: any
]: nothing -> record<name: string, wall_ms: float, wall_display: any, peak_live_mb: any, peak_upper_mb: any, total_allocated_mb: any> {
  {
    name: $name
    wall_ms: $wall_ms
    wall_display: $wall_display
    peak_live_mb: $peak_live_mb
    peak_upper_mb: $peak_upper_mb
    total_allocated_mb: $total_allocated_mb
  }
}

# Runs one benchmark and returns a benchmark-result record. All three
# languages' benchmark executables print output in a shared shape --
# "Wall time=<value> <unit>," and (Haskell/C# only) "memory allocated=<value>
# MB." -- so the same parsing works across all of them, and a metric a given
# language doesn't report simply comes back null (rendered as "-").
#
# `floor` controls whether/how a heaptrack-based peak_memory_upper_bound is
# computed (see "Methodology for measuring peak memory usage" in
# BENCHMARKS.md):
#   - a number: subtract this app-init floor from heaptrack's peak, then add
#     back peak_live_rts_memory (Haskell's upper-bound estimate, since its GC
#     heap is invisible to heaptrack).
#   - 0.0: no floor to subtract, just report heaptrack's raw peak (Rust,
#     whose default allocator is malloc-backed so heaptrack sees ~all of it).
#   - null: skip heaptrack entirely, leave peak_memory_upper_bound as null
#     (C#, whose GC.GetTotalMemory can't be trusted as a true peak the way
#     GHC's max_live_bytes can -- see BENCHMARKS.md).
def run-bench [
  bench_name: string
  floor: any
  program: string
  ...args: string
]: nothing -> record {
  print $bench_name

  let result = (^$program ...$args | complete)
  let output = $"($result.stdout)($result.stderr)"

  let wall_display = (extract-wall-time $output)
  let wall_ms = if $wall_display == null { 0.0 } else { time-to-ms $wall_display }
  let peak_live_mb = (extract-peak-live-mb $output)
  let total_allocated_mb = (extract-total-allocated-mb $output)

  let peak_upper_mb = if $floor == null {
    null
  } else {
    ^heaptrack --record-only -o benchmark-results/heaptrack.outdat $program ...$args e> /dev/null
    let heap_mb = (heaptrack-peak-mb benchmark-results/heaptrack.outdat.zst)
    mv benchmark-results/heaptrack.outdat.zst $"benchmark-results/($bench_name).outdat.zst"
    # Rounded to 1 decimal place: the addition/subtraction above otherwise
    # surfaces IEEE754 noise (e.g. "2.109999999999985") in the final table.
    (($peak_live_mb | default 0.0) + $heap_mb - $floor) | math round --precision 1
  }

  new-benchmark-result $bench_name $wall_ms $wall_display $peak_live_mb $peak_upper_mb $total_allocated_mb
}

# nushell has no enum/sum-type declaration and no union types in signatures
# (`float | nothing` is a parse error, not just unchecked) -- `lang` is a
# plain string, and this is the one place its three valid values ("Haskell",
# "Rust", "Csharp") are enumerated and checked, with a wildcard branch that
# fails loudly on anything else. This is as close to an enum as the language
# gets: a runtime-checked value, not a compile-time-checked type.
#
# Builds the (program, args, floor) invocation for one benchmark entry,
# dispatching on its language -- this is the one place that knows how each
# language's executable is invoked and how its peak_memory_upper_bound
# should be computed (see run-bench's doc comment for what `floor` means).
def bench-invocation [
  entry: record<name: string, lang: string>
  exes: record<haskell: string, rust: string, csharp: string, haskell_floor: float>
]: nothing -> record<program: string, args: list<string>, floor: any> {
  match $entry.lang {
    "Haskell" => {program: $exes.haskell, args: ["--match", $entry.name], floor: $exes.haskell_floor}
    "Rust" => {program: $exes.rust, args: [], floor: 0.0}
    "Csharp" => {program: "dotnet", args: [$exes.csharp], floor: null}
    _ => (error make {msg: $"unknown benchmark language: ($entry.lang)"})
  }
}

# Runs run-bench once per {name, lang} entry in a group, dispatching each to
# its own language's executable/invocation/floor via bench-invocation. This
# is what lets Haskell, Rust and C# benchmarks (e.g. all five entries in
# record_stream_bench) share one run-bench-group call instead of needing a
# separate run-bench call per language.
def run-bench-group [
  exes: record<haskell: string, rust: string, csharp: string, haskell_floor: float>
  entries: list<record<name: string, lang: string>>
]: nothing -> list {
  $entries | each {|e|
    let inv = (bench-invocation $e $exes)
    run-bench $e.name $inv.floor $inv.program ...$inv.args
  }
}

# Italicizes hpgsql's own row so it stands out against the libraries it's
# compared to.
def display-name [name: string]: nothing -> string {
  if ($name | str starts-with "hpgsql") { $"*($name)*" } else { $name }
}

def render-row [b: record]: nothing -> string {
  $"| (display-name $b.name) | ($b.wall_display) | (fmt-mb $b.peak_live_mb) | (fmt-mb $b.peak_upper_mb) | (fmt-mb $b.total_allocated_mb) |"
}

# Writes a markdown table file, sorted slowest-to-fastest by wall-clock time.
def write-table [path: string, results: list]: nothing -> nothing {
  let sorted = ($results | sort-by wall_ms --reverse)
  let lines = [$TABLE_HEADER, $TABLE_SEPARATOR] ++ ($sorted | each {|b| render-row $b })
  $lines | str join "\n" | save -f $path
}

def main [] {
  let record_list_bench = [
    {name: "postgresql-simple Record List (100000 rows, Generically derived row decoder)", lang: "Haskell"}
    {name: "hasql Record List (100000 rows)", lang: "Haskell"}
    {name: "hpgsql Record List (100000 rows, Generically derived row decoder)", lang: "Haskell"}
  ]
  let tuple_list_bench = [
    {name: "postgresql-simple Tuple List (100000 rows)", lang: "Haskell"}
    {name: "hasql Tuple List (100000 rows)", lang: "Haskell"}
    {name: "hpgsql Tuple List (100000 rows)", lang: "Haskell"}
  ]
  # rust-bench and the Npgsql benchmark both mirror hpgsql's Record Stream
  # benchmark (same 17 columns, streamed and discarded as they arrive), so
  # their results join this same table.
  let record_stream_bench = [
    {name: "streaming-postgresql-simple Record Stream (100000 rows, Generically derived row decoder)", lang: "Haskell"}
    {name: "postgresql-simple Record fold (100000 rows, Generically derived row decoder)", lang: "Haskell"}
    {name: "hpgsql Record Stream (100000 rows, Generically derived row decoder)", lang: "Haskell"}
    {name: "rust-tokio-postgres Record Stream (100000 rows)", lang: "Rust"}
    {name: "Npgsql Record Stream (100000 rows)", lang: "Csharp"}
  ]
  let tuple_stream_bench = [
    {name: "streaming-postgresql-simple Tuple Stream (100000 rows)", lang: "Haskell"}
    {name: "postgresql-simple Tuple fold (100000 rows)", lang: "Haskell"}
    {name: "hpgsql Tuple Stream (100000 rows)", lang: "Haskell"}
  ]
  let copy_bench = [
    {name: "postgresql-simple text COPY (100000 rows)", lang: "Haskell"}
    {name: "hpgsql copyFromS binary COPY (100000 rows)", lang: "Haskell"}
  ]

  # Compile executables
  cabal build hpgsql-benchmarks
  cargo build --release --manifest-path rust-bench/Cargo.toml
  dotnet build -c Release csharp-benchmarks/CsharpBenchmarks.csproj

  # Wipe the folder, recreate it and run the benchmarks
  rm -rf benchmark-results
  mkdir benchmark-results
  let benchexe = (cabal list-bin hpgsql-benchmarks | str trim)
  let rust_benchexe = "./rust-bench/target/release/rust-bench"
  # Invoked via `dotnet <dll>` rather than the native apphost binary
  # directly: the apphost can't locate libhostfxr.so outside of a `dotnet
  # run`/`dotnet exec` context (e.g. under nix-shell), and fails silently as
  # far as this script is concerned (its stdout carries only an error
  # message, so extract-wall-time et al. would all come back null instead of
  # erroring loudly).
  let csharp_dll = "./csharp-benchmarks/bin/Release/net8.0/CsharpBenchmarks.dll"

  # Measure the app-init memory floor once for the whole run: this matches no
  # benchmark name, so hspec starts up the GHC RTS and immediately exits
  # without running any benchmark's DB connections/queries. This floor is
  # workload-independent (see BENCHMARKS.md), so collecting it once and
  # reusing it for every benchmark below is both cheaper and more correct
  # than remeasuring it per benchmark.
  ^heaptrack --record-only -o benchmark-results/heaptrack.outdat $benchexe --match "no benchmark name matches this" e> /dev/null
  let app_init_peak_mb = (heaptrack-peak-mb benchmark-results/heaptrack.outdat.zst)
  mv benchmark-results/heaptrack.outdat.zst benchmark-results/app-init-baseline.outdat.zst
  print $"App-init heap memory floor \(subtracted from every Haskell benchmark's heaptrack peak below\): ($app_init_peak_mb) M"

  let exes = {
    haskell: $benchexe
    rust: $rust_benchexe
    csharp: $csharp_dll
    haskell_floor: $app_init_peak_mb
  }

  let record_list_results = (run-bench-group $exes $record_list_bench)
  let tuple_list_results = (run-bench-group $exes $tuple_list_bench)
  let record_stream_results = (run-bench-group $exes $record_stream_bench)
  let tuple_stream_results = (run-bench-group $exes $tuple_stream_bench)
  let copy_results = (run-bench-group $exes $copy_bench)

  write-table benchmark-results/record_list_bench.md $record_list_results
  write-table benchmark-results/tuple_list_bench.md $tuple_list_results
  write-table benchmark-results/record_stream_bench.md $record_stream_results
  write-table benchmark-results/tuple_stream_bench.md $tuple_stream_results
  write-table benchmark-results/copy_bench.md $copy_results
}
