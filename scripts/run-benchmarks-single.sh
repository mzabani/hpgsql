#!/usr/bin/env bash
cabal build hpgsql-benchmarks
$(cabal list-bin hpgsql-benchmarks) "$@"
