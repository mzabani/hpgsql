#!/usr/bin/env bash
set -eo pipefail

cabal build hpgsql-benchmarks
$(cabal list-bin hpgsql-benchmarks) "$@"
