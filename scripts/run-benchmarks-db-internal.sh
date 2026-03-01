#!/usr/bin/env bash

`cabal list-bin -O2 hpgsql-benchmarks` "${@}"
