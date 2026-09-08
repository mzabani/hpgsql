#!/usr/bin/env bash
cargo build --release --manifest-path rust-bench/Cargo.toml
./rust-bench/target/release/rust-bench "$@"
