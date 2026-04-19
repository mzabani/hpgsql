#!/usr/bin/env bash
$(cabal list-bin -O1 hpgsql-benchmarks) "$@"
