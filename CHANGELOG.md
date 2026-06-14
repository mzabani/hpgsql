## v0.2.0

- Fixed a bug where an asynchronous exception thrown at the right time could hide an `IrrecoverableHpgsqlError` when using `withTransaction`. Thank you Yuras for the report.
- SCRAM-SHA-256 authentication implemented.
- Added `connectionIsClosed` function.
- `resetConnectionState` made much more thorough, to the image of the `DISCARD ALL` statement.
- `pipelineMay` and `pipelineMayWith` publicly exported.
