- Add a bunch more types and instances
  - hstore
  - PGRange?
- Get rid of Time.hs? Move Unbounded to Types.hs?
- A newtype that changes `ToPgRow` to encode `Int` as an `Int32`?

## Less important:
- Experiment with TCP NoDelay
- Connecting with TLS encrypted connection (can come later?)
- Connecting should use PGUSER, PGDATABASE, PGPORT, etc., when the equivalent values are not provided. Or should it?
