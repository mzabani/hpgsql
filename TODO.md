- Write documentation
- Add a bunch more types and instances
  - hstore
  - PGRange?
- Get rid of Time.hs? Move Unbounded to Types.hs?
- Support escapeIdentifier taking schema-qualified objects
- Review all exposed functions per module, move more to Internal modules
- Double-check our tests don't import Internal modules except for very specific things
    - Otherwise we might accidentally not be exposing functions in public modules
- Read https://www.iankduncan.com/engineering/2024-01-26-records-of-effects, think about it.

## Less important:
- Experiment with TCP NoDelay
- Connecting with TLS encrypted connection (can come later?)
- Connecting should use PGUSER, PGDATABASE, PGPORT, etc., when the equivalent values are not provided. Or should it?
