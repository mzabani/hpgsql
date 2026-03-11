#!/usr/bin/env bash

export DATABASE_CONNSTRING="user=$PGUSER dbname=$PGDATABASE host=$PGHOST port=$PGPORT"
`cabal list-bin hpgsql-simple-compat-tests` "${@}"
