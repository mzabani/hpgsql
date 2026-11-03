## v0.4.0

This is a performance improvement and bugfixing release. Users are highly encouraged to upgrade.

- `vALUES` was encoding the list in reverse order, and that has been fixed. Shouldn't have affected order-insensitive queries, but is embarrassing.
- Fix a connection hanging if postgres killed the connection abruptly. Now a proper exception is thrown.
- Fix a parameter interpolation bug that caused an infinite loop in user-space, before queries were even sent to postgres.
- Fix COPY deadlocking in cases of wrong COPY syntax.
- Fix wrong numeric scale as per the postgres `scale(numeric)` function.
- In terms of raw row decoding performance, we now beat Npgsql and are close to tokio-postgres in one benchmark. Read more below.

No breaking API changes in non-Internal modules in this version, but we now recommend using `fieldRowDecoder` instead of `singleField fieldDecoder` where you can. They're equivalent when compiling with -O1 due to rewrite rules, but the former is more performant when compiling with no optimizations.

## v0.3.0
- Support GHC 9.12
- Support OverloadedRecordDot inside the `sql` quasiquoter
- Fix the lack of type-checking query fields when using RowDecoderMonadic
- Also when using RowDecoderMonadic, a previously cryptic error message has been improved
- Performance materializing query results improved by ~23% in some benchmarks
- Binary COPY made ~4.3% faster
- Users can supply non-default connection options, including the minimum socket recv size
- Dropped dependencies cereal and transformers
- Replaced dependency haskell-src-meta with ghc-lib-parser

To support OverloadedRecordDot in the `sql` quasiquoter, we have a brand new implementation of a Haskell expression parser instead of using haskell-src-meta's. Parts of the Haskell language might no longer be supported as parameters in the quasiquoter, but the more commonly used ones should be there, and error messages should be helpful to guide you in case you run into an unsupported case.

Thank you Brandon Chinn and Nick Ivanych for your contributions.

## v0.2.0.1

- Major fix: connecting via TCP on MacOS could fail completely. Thanks @luntain for the contribution.
- Performance materializing query results improved by ~17%
- Future-proofing for a future protocol change: accepting longer backend secret keys.

## v0.2.0.0

- Fixed a bug where an asynchronous exception thrown at the right time could hide an `IrrecoverableHpgsqlError` when using `withTransaction`. Thank you Yuras for the report.
- SCRAM-SHA-256 authentication implemented.
- Added `connectionIsClosed` function.
- `resetConnectionState` made much more thorough, to the image of the `DISCARD ALL` statement.
- `pipelineMay` and `pipelineMayWith` publicly exported.
