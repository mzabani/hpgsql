use chrono::{DateTime, NaiveDate, Utc};
use futures_util::{pin_mut, TryStreamExt};
use rust_decimal::Decimal;
use std::env;
use std::hint::black_box;
use std::time::Instant;
use tokio_postgres::types::ToSql;
use tokio_postgres::NoTls;

// Mirrors hpgsql-benchmarks/src/Main.hs's BenchRow / sql17 query, and its
// benchmark methodology: 2 concurrent connections, each running the query
// once per round, repeated for 10 rounds, with total wall-clock time
// reported across all 10 rounds (see `bench`/`withMultipleConnections` in
// that file).
#[derive(Debug)]
#[allow(dead_code)]
struct BenchRow {
    id: i32,
    date1: NaiveDate,
    date2: NaiveDate,
    timestamp1: DateTime<Utc>,
    timestamp2: DateTime<Utc>,
    text1: String,
    text2: String,
    double1: f64,
    double2: f64,
    maybe_int: Option<i32>,
    maybe_text: Option<String>,
    maybe_double: Option<f64>,
    maybe_date: Option<NaiveDate>,
    numeric: Decimal,
    float: f32,
    bool1: bool,
    bool2: bool,
}

const SQL17: &str = "SELECT g, ('2000-01-01'::date + g::int4), ('2000-06-15'::date + g::int4), \
    ('2000-01-01T00:00:00Z'::timestamptz + g * interval '1 second'), \
    ('2020-06-15T12:00:00Z'::timestamptz + g * interval '1 minute'), \
    'row-' || g::text, 'item-' || g::text, g::float8 * 1.5, g::float8 * 2.5, \
    NULL::int4, NULL::text, NULL::float8, NULL::date, \
    g::numeric, g::float4, g%2=0, g%2=1 \
    FROM generate_series(1,$1) g";

const N: i32 = 100_000;
const NUM_CONCURRENT_CONNECTIONS: usize = 2;
const NUM_ROUNDS: usize = 10;

// Matches Main.hs's/Program.cs's "Wall time=<value> <unit>," convention (a
// space before the unit, unlike Duration's Debug format) so the benchmark
// runner script can grep all three languages' output with the same pattern.
fn format_secs(s: f64) -> String {
    if s < 0.001 {
        format!("{:.1} μs", s * 1_000_000.0)
    } else if s < 1.0 {
        format!("{:.1} ms", s * 1000.0)
    } else {
        format!("{:.3} s", s)
    }
}

fn conn_string() -> Result<String, Box<dyn std::error::Error + Send + Sync>> {
    let host = env::var("PGHOST")?;
    let port: u16 = env::var("PGPORT")?.parse()?;
    let dbname = env::var("PGDATABASE")?;
    let user = env::var("PGUSER")?;
    Ok(format!("host={host} port={port} dbname={dbname} user={user}"))
}

// Connects fresh, prepares and streams SQL17's results once (decoding each
// row as it arrives, never collecting them into a Vec), then drops the
// connection -- mirroring `acquireConn` + one `querySWith`/`S.effects` call +
// `closeConn` in `withMultipleConnections`.
async fn run_once(conn_string: String) -> Result<usize, Box<dyn std::error::Error + Send + Sync>> {
    let (client, connection) = tokio_postgres::connect(&conn_string, NoTls).await?;

    let connection_task = tokio::spawn(connection);

    let stmt = client.prepare(SQL17).await?;
    let params: [&(dyn ToSql + Sync); 1] = [&N];
    let stream = client.query_raw(&stmt, params).await?;
    pin_mut!(stream);

    let mut count = 0usize;
    while let Some(row) = stream.try_next().await? {
        // Decode every field, same as materializing a BenchRow would, but
        // discard it immediately instead of collecting into a Vec.
        black_box(BenchRow {
            id: row.get(0),
            date1: row.get(1),
            date2: row.get(2),
            timestamp1: row.get(3),
            timestamp2: row.get(4),
            text1: row.get(5),
            text2: row.get(6),
            double1: row.get(7),
            double2: row.get(8),
            maybe_int: row.get(9),
            maybe_text: row.get(10),
            maybe_double: row.get(11),
            maybe_date: row.get(12),
            numeric: row.get(13),
            float: row.get(14),
            bool1: row.get(15),
            bool2: row.get(16),
        });
        count += 1;
    }

    drop(client);
    let _ = connection_task.await;

    Ok(count)
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let conn_string = conn_string()?;

    // Warm up postgres before any timed measurement, mirroring the Haskell
    // benchmark's warm-up connection.
    {
        let (client, connection) = tokio_postgres::connect(&conn_string, NoTls).await?;
        let connection_task = tokio::spawn(connection);
        client.execute("SELECT * FROM generate_series(1,100000)", &[]).await?;
        drop(client);
        let _ = connection_task.await;
    }

    println!(
        "Running {NUM_ROUNDS} rounds of {NUM_CONCURRENT_CONNECTIONS} concurrent connections each running the query once..."
    );

    let start = Instant::now();
    let mut total_rows = 0usize;
    for _round in 0..NUM_ROUNDS {
        let tasks: Vec<_> = (0..NUM_CONCURRENT_CONNECTIONS)
            .map(|_| tokio::spawn(run_once(conn_string.clone())))
            .collect();
        for task in tasks {
            total_rows += task.await??;
        }
    }
    let elapsed = start.elapsed();

    println!(
        "--- Benchmark rust-tokio-postgres Record Stream: Wall time={}, decoded {total_rows} rows total across {NUM_ROUNDS} rounds.",
        format_secs(elapsed.as_secs_f64())
    );

    Ok(())
}
