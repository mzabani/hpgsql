#!/usr/bin/env bash
set -eo pipefail
cargo build --release --manifest-path rust-bench/Cargo.toml
./rust-bench/target/release/rust-bench "$@"
