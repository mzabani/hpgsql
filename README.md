[![CI](https://github.com/mzabani/hpgsql/actions/workflows/main.yml/badge.svg)](https://github.com/mzabani/hpgsql/actions/workflows/main.yml)

Hpgsql is a PostgreSQL driver written in pure Haskell (no libpq), with an API largely inspired by the great [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple) library, but featuring:

- Usage of PostgreSQL's binary protocol
- Query arguments passed via the protocol instead of being escaped into the query string
- Pipeline support
- Capable of streaming query results directly from the socket (not with cursors), and ability to use Streams in pipelines
- [Interruption safety](/INTERRUPTION-SAFETY.md)
- Reasonable thread safety; users can assume thread safety everywhere unless specific (and documented) instructions say otherwise
- A SQL quasiquoter like the one in [postgresql-query](https://hackage.haskell.org/package/postgresql-query) and [hasql-interpolate](https://hackage-content.haskell.org/package/hasql-interpolate-1.0.1.0/docs/Hasql-Interpolate.html)
- Prepared statements (TODO, actually)

### Migrating from postgresql-simple

This repository contains a fork of [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple) that preserves as much as possible** the names of modules, functions, types, classes, exceptions, etc. Its purpose is to ease migrating to hpgsql, and I intend to fully support its development to make it and keep it as similar to postgresql-simple as it can be.

It is called hpgsql-simple-compat, and its implementation uses hpgsql. You can get a `HPgConnection` out of it so you can gradually migrate your queries to hpgsql.

It also contains parts of [postgresql-libpq](https://hackage.haskell.org/package/postgresql-libpq) and even of [postgresql-query](https://hackage.haskell.org/package/postgresql-query), all of them also implemented on top of hpgsql.

- <sub>I haven't been able to preserve _everything_, so some differences do exist. Also the library is not feature complete yet.</sub>
- <sub>There may be small intentional differences added to help the transition, like a `sqlStatement` field in the `SqlError` exception so it's easier to know which queries are failing.</sub>

### Performance

Some benchmarks show materializing large query results with hpgsql takes \~28% the time postgresql-simple takes, and \~52% the time hasql takes (on my computer, Linux x64, GHC 9.10.3, compiled with -O1). Memory usage (only the one allocated by GHC's runtime, so not counting FFI/C allocated memory) is \~37% that of postgresql-simple, but still \~152% that of hasql (though again it's not clear what total allocated memory, that is, including libpq's allocations, is).

When comparing hpgsql's Stream querying, hpgsql takes \~13% the time of [streaming-postgresql-simple](https://hackage.haskell.org/package/streaming-postgresql-simple) and \~27% the memory (not counting FFI/C allocations).

Sadly, hpgsql's binary COPY runs in about the same time as postgresql-simple's textual COPY, and uses 244% of postgresql-simple's memory (same caveat as always about FFI/C allocated memory).

See [BENCHMARKS.md](/BENCHMARKS.md) for more details.
