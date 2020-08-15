#!/usr/bin/env bash

export DATABASE_CONNSTRING="user=postgres dbname=postgres host=127.0.0.1 port=$PGPORT"
`cabal list-bin -O0 hpgsql-simple-compat:test` "${@}"
