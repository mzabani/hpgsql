- Write documentation
- Add a bunch more types and instances
  - TimeOfDay
  - hstore
  - PGRange?
- Prepared statements
- Make FieldParser implement `Alternative`, add `FromPgField (Either a b)` instance.
- `run format-hs`, check all is formatted in CI as the last step
- Write a FromRow instance in hpgsql-simple-compat that checks number of fields and decodes a row of arbitrary length
  - Can the Applicative RowParser also know about remaining fields? Ordering is fine in Applicative, it's just sequencing that isn't.
- Connecting with a password
- Change version to 0.1 everywhere
- Double-check licensing


## Less important:
- Experiment with TCP NoDelay
- Connecting with TLS encrypted connection (can come later?)
- Import postgresql-query into repository? Not sure.
