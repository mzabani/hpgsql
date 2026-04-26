- Write documentation
- Add a bunch more types and instances
  - TimeOfDay
  - hstore
  - PGRange?
- Make FieldParser implement `Alternative`, add `FromPgField (Either a b)` instance.
- `run format-hs`, check all is formatted in CI as the last step
- Write a FromRow instance in hpgsql-simple-compat that checks number of fields and decodes a row of arbitrary length
  - Can the Applicative RowParser also know about remaining fields? Ordering is fine in Applicative, it's just sequencing that isn't.- Review all exposed functions per module, move more to Internal modules
- Double-check our tests don't import Internal modules except for very specific things
    - Otherwise we might accidentally not be exposing functions in public modules
- Do we need to depend on `transformers`?
- Do we need both liftQueryStatic and liftQueryDynamic?
- Add test that sends a pipeline with a massive number of queries (does postgres keep receiving if we're not consuming results?)
- Add pipelineMay and queryMay
- Connecting with a password
- Double-check licensing


## Less important:
- Experiment with TCP NoDelay
- Connecting with TLS encrypted connection (can come later?)
- Import postgresql-query into repository? Not sure.
- Connecting should use PGUSER, PGDATABASE, PGPORT, etc., when the equivalent values are not provided.
