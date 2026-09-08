use futures_util::{pin_mut, TryStreamExt};
use rust_bench::{conn_string, format_secs, warmup, BenchRow, N, NUM_CONCURRENT_CONNECTIONS, NUM_ROUNDS, SQL17};
use std::hint::black_box;
use std::time::Instant;
use tokio_postgres::types::ToSql;
use tokio_postgres::NoTls;

async fn run_once(conn_string: String) -> Result<usize, Box<dyn std::error::Error + Send + Sync>> {
    let (client, connection) = tokio_postgres::connect(&conn_string, NoTls).await?;

    let connection_task = tokio::spawn(connection);

    let stmt = client.prepare(SQL17).await?;
    let params: [&(dyn ToSql + Sync); 1] = [&N];
    let stream = client.query_raw(&stmt, params).await?;
    pin_mut!(stream);

    let mut bench_rows: Vec<BenchRow> = Vec::new();
    while let Some(row) = stream.try_next().await? {
        bench_rows.push(BenchRow::from_row(&row));
    }
    let count = bench_rows.len();
    black_box(bench_rows);

    drop(client);
    let _ = connection_task.await;

    Ok(count)
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let conn_string = conn_string()?;
    warmup(&conn_string).await?;

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
        "--- Benchmark rust-tokio-postgres Record List: Wall time={}, decoded {total_rows} rows total across {NUM_ROUNDS} rounds.",
        format_secs(elapsed.as_secs_f64())
    );

    Ok(())
}
