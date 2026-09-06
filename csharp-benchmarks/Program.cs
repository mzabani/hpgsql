using System.Data;
using System.Diagnostics;
using Npgsql;

const int NumConcurrentConnections = 2;
const int NumRuns = 10;

string BuildConnectionString()
{
    var host = Environment.GetEnvironmentVariable("PGHOST");
    var port = Environment.GetEnvironmentVariable("PGPORT") ?? "5432";
    var database = Environment.GetEnvironmentVariable("PGDATABASE") ?? "postgres";
    var user = Environment.GetEnvironmentVariable("PGUSER") ?? Environment.UserName;
    // An empty or unset PGHOST means connect via localhost (matching libpq behavior
    // when the host is empty and no unix socket directory is found).
    if (string.IsNullOrEmpty(host))
        host = "localhost";
    return $"Host={host};Port={port};Database={database};Username={user}";
}

async Task<NpgsqlConnection> ConnectAsync()
{
    var conn = new NpgsqlConnection(BuildConnectionString());
    await conn.OpenAsync();
    return conn;
}

async Task<List<T>> WithMultipleConnections<T>(int n, Func<NpgsqlConnection, Task<T>> f)
{
    var barrier = new CountdownEvent(n);
    var tasks = Enumerable.Range(0, n).Select(_ => Task.Run(async () =>
    {
        await using var conn = await ConnectAsync();
        barrier.Signal();
        barrier.Wait();
        return await f(conn);
    })).ToArray();
    var results = await Task.WhenAll(tasks);
    return results.ToList();
}

async Task Bench(string name, Func<Task> action)
{
    // Match the Haskell benchmark methodology (see `bench` in
    // hpgsql-benchmarks/src/Main.hs): run all rounds back-to-back with no
    // explicit GC in between (only whatever the GC decides to do on its
    // own), then a single blocking GC after the last round, all included in
    // the timed wall clock.
    var allocBefore = GC.GetTotalAllocatedBytes(precise: true);
    var sw = Stopwatch.StartNew();
    for (int i = 0; i < NumRuns; i++)
    {
        await action();
    }
    GC.Collect();
    GC.WaitForPendingFinalizers();
    GC.Collect();
    sw.Stop();
    var allocAfter = GC.GetTotalAllocatedBytes(precise: true);
    var allocMB = (allocAfter - allocBefore) / (1024.0 * 1024.0);

    Console.WriteLine(
        $"--- Benchmark {name}: Wall time={FormatSecs(sw.Elapsed.TotalSeconds)}"
        + $", total .NET (does not include native heap) memory allocated={allocMB:F1} MB.");
}

string FormatSecs(double s)
{
    if (s < 0.001) return $"{s * 1_000_000:F1} μs";
    if (s < 1.0) return $"{s * 1000:F1} ms";
    return $"{s:F3} s";
}

// --- Main ---

Console.WriteLine($"IMPORTANT: all measurements collected over {NumRuns} runs of each benchmark");
if (NumConcurrentConnections > 1)
    Console.WriteLine(
        $"IMPORTANT: all benchmarks involve running the benchmarked query in "
        + $"{NumConcurrentConnections} connections in parallel");

// Warmup postgres with the same generate_series query
await using (var warmupConn = await ConnectAsync())
{
    await using var warmupCmd = new NpgsqlCommand("SELECT * FROM generate_series(1,100000)", warmupConn);
    await warmupCmd.ExecuteNonQueryAsync();
}
GC.Collect();
GC.WaitForPendingFinalizers();
GC.Collect();

// Used to measure this binary's own app-init memory floor (CLR + Npgsql JIT +
// warmup connection), mirroring how the Haskell benchmark executable's floor
// is measured via `--match "no benchmark name matches this"`: exit here,
// after warmup but before the timed benchmark runs.
if (Environment.GetEnvironmentVariable("CSHARP_BENCH_FLOOR_ONLY") == "1")
    return;

var peakMemBefore = Process.GetCurrentProcess().PeakWorkingSet64;
var liveBytesBefore = GC.GetTotalMemory(forceFullCollection: true);

// The same 17-column query as the Haskell/Rust "Record Stream" benchmarks
// (sql17 in Main.hs / SQL17 in rust-bench/src/main.rs), so all three
// participants in that comparison decode the same columns. Columns are read
// by ordinal below (no ORM/reflection involved), so no aliases are needed.
const string sql = """
    SELECT g,
           ('2000-01-01'::date + g::int4),
           ('2000-06-15'::date + g::int4),
           ('2000-01-01T00:00:00Z'::timestamptz + g * interval '1 second'),
           ('2020-06-15T12:00:00Z'::timestamptz + g * interval '1 minute'),
           'row-' || g::text,
           'item-' || g::text,
           g::float8 * 1.5,
           g::float8 * 2.5,
           NULL::int4,
           NULL::text,
           NULL::float8,
           NULL::date,
           g::numeric,
           g::float4,
           g%2=0,
           g%2=1
    FROM generate_series(1, @n) g
    """;

{
    const int n = 100_000;
    await Bench($"Npgsql Record Stream ({n} rows)", async () =>
    {
        await WithMultipleConnections(NumConcurrentConnections, async conn =>
        {
            await using var cmd = new NpgsqlCommand(sql, conn);
            cmd.Parameters.AddWithValue("n", n);
            // SequentialAccess streams the reader forward-only without
            // buffering whole rows, the closest equivalent to the Haskell
            // streaming test (and to rust-bench's query_raw). Columns must be
            // read in increasing ordinal order under this mode, which the
            // BenchRow construction below does.
            await using var reader = await cmd.ExecuteReaderAsync(CommandBehavior.SequentialAccess);
            while (await reader.ReadAsync())
            {
                var row = new BenchRow
                {
                    g = reader.GetInt32(0),
                    date1 = reader.GetFieldValue<DateOnly>(1),
                    date2 = reader.GetFieldValue<DateOnly>(2),
                    timestamp1 = reader.GetFieldValue<DateTime>(3),
                    timestamp2 = reader.GetFieldValue<DateTime>(4),
                    text1 = reader.GetString(5),
                    text2 = reader.GetString(6),
                    double1 = reader.GetDouble(7),
                    double2 = reader.GetDouble(8),
                    maybe_int = await reader.IsDBNullAsync(9) ? null : reader.GetInt32(9),
                    maybe_text = await reader.IsDBNullAsync(10) ? null : reader.GetString(10),
                    maybe_double = await reader.IsDBNullAsync(11) ? null : reader.GetDouble(11),
                    maybe_date = await reader.IsDBNullAsync(12) ? null : reader.GetFieldValue<DateOnly>(12),
                    numeric1 = reader.GetFieldValue<decimal>(13),
                    float1 = reader.GetFloat(14),
                    bool1 = reader.GetBoolean(15),
                    bool2 = reader.GetBoolean(16),
                };
                // Discard the row as it arrives instead of collecting it,
                // same as hpgsql's/rust-bench's streaming benchmarks.
                _ = row;
            }
            return 0;
        });
    });
}

GC.Collect();
GC.WaitForPendingFinalizers();
GC.Collect();

var peakMemAfter = Process.GetCurrentProcess().PeakWorkingSet64;
var liveBytesAfter = GC.GetTotalMemory(forceFullCollection: true);

double ToMB(long bytes) => bytes / (1024.0 * 1024.0);
Console.WriteLine($"--- Peak memory (PeakWorkingSet64): {ToMB(peakMemAfter - peakMemBefore):F1} M");
Console.WriteLine($"--- Peak live data (GC.GetTotalMemory): {ToMB(liveBytesAfter - liveBytesBefore):F1} M");

// Row type matching the 17-column query, decoded field-by-field via
// NpgsqlDataReader above (no ORM/reflection involved).
public class BenchRow
{
    public int g { get; set; }
    public DateOnly date1 { get; set; }
    public DateOnly date2 { get; set; }
    public DateTime timestamp1 { get; set; }
    public DateTime timestamp2 { get; set; }
    public string text1 { get; set; } = "";
    public string text2 { get; set; } = "";
    public double double1 { get; set; }
    public double double2 { get; set; }
    public int? maybe_int { get; set; }
    public string? maybe_text { get; set; }
    public double? maybe_double { get; set; }
    public DateOnly? maybe_date { get; set; }
    public decimal numeric1 { get; set; }
    public float float1 { get; set; }
    public bool bool1 { get; set; }
    public bool bool2 { get; set; }
}
