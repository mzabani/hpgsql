#!/usr/bin/env bash

`cabal list-bin -O0 hpgsql-tests` "${@}"
