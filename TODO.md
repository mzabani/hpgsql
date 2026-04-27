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
- Do we need both liftQueryStatic and liftQueryDynamic?
- Read https://www.iankduncan.com/engineering/2024-01-26-records-of-effects, think about it.
- Connecting with a password
- Double-check licensing and authorship in cabal files and READMEs


## Less important:
- Experiment with TCP NoDelay
- Connecting with TLS encrypted connection (can come later?)
- Import postgresql-query into repository? Not sure.
- Connecting should use PGUSER, PGDATABASE, PGPORT, etc., when the equivalent values are not provided.
