use chrono::{DateTime, NaiveDate, Utc};
use rust_decimal::Decimal;
use std::env;
use tokio_postgres::Row;

// Mirrors hpgsql-benchmarks/src/Main.hs's BenchRow / sql17 query, and its
// benchmark methodology: 2 concurrent connections, each running the query
// once per round, repeated for 10 rounds, with total wall-clock time
// reported across all 10 rounds (see `bench`/`withMultipleConnections` in
// that file).
#[derive(Debug)]
#[allow(dead_code)]
pub struct BenchRow {
    pub id: i32,
    pub date1: NaiveDate,
    pub date2: NaiveDate,
    pub timestamp1: DateTime<Utc>,
    pub timestamp2: DateTime<Utc>,
    pub text1: String,
    pub text2: String,
    pub double1: f64,
    pub double2: f64,
    pub maybe_int: Option<i32>,
    pub maybe_text: Option<String>,
    pub maybe_double: Option<f64>,
    pub maybe_date: Option<NaiveDate>,
    pub numeric: Decimal,
    pub float: f32,
    pub bool1: bool,
    pub bool2: bool,
}

impl BenchRow {
    pub fn from_row(row: &Row) -> Self {
        BenchRow {
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
        }
    }
}

pub const SQL17: &str = "SELECT g, ('2000-01-01'::date + g::int4), ('2000-06-15'::date + g::int4), \
    ('2000-01-01T00:00:00Z'::timestamptz + g * interval '1 second'), \
    ('2020-06-15T12:00:00Z'::timestamptz + g * interval '1 minute'), \
    'row-' || g::text, 'item-' || g::text, g::float8 * 1.5, g::float8 * 2.5, \
    NULL::int4, NULL::text, NULL::float8, NULL::date, \
    g::numeric, g::float4, g%2=0, g%2=1 \
    FROM generate_series(1,$1) g";

pub const N: i32 = 100_000;
pub const NUM_CONCURRENT_CONNECTIONS: usize = 2;
pub const NUM_ROUNDS: usize = 10;

// Matches Main.hs's/Program.cs's "Wall time=<value> <unit>," convention (a
// space before the unit, unlike Duration's Debug format) so the benchmark
// runner script can grep all three languages' output with the same pattern.
pub fn format_secs(s: f64) -> String {
    if s < 0.001 {
        format!("{:.1} μs", s * 1_000_000.0)
    } else if s < 1.0 {
        format!("{:.1} ms", s * 1000.0)
    } else {
        format!("{:.3} s", s)
    }
}

pub fn conn_string() -> Result<String, Box<dyn std::error::Error + Send + Sync>> {
    let host = env::var("PGHOST")?;
    let port: u16 = env::var("PGPORT")?.parse()?;
    let dbname = env::var("PGDATABASE")?;
    let user = env::var("PGUSER")?;
    Ok(format!("host={host} port={port} dbname={dbname} user={user}"))
}

// Warms up postgres before any timed measurement, mirroring the Haskell
// benchmark's warm-up connection.
pub async fn warmup(conn_string: &str) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let (client, connection) = tokio_postgres::connect(conn_string, tokio_postgres::NoTls).await?;
    let connection_task = tokio::spawn(connection);
    client
        .execute("SELECT * FROM generate_series(1,100000)", &[])
        .await?;
    drop(client);
    let _ = connection_task.await;
    Ok(())
}
