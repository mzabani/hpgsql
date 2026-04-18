Imagine you have concurrent code like this:

```haskell
flip onException logErrorToDatabase $
  runConcurrently $ (,) <$> Concurrently (readSomeFileFromDisk >> runSomeQuery) <*> Concurrently doSomethingElse
```

What would happen if `doSomethingElse` threw an exception _while_ `runSomeQuery` was still running? `runConcurrently` would send an asynchronous exception to the other thread, which would interrupt the query in the middle. Still, in Hpgsql, `logErrorToDatabase` would work fine, because Hpgsql is interruption-safe.

This is not necessarily true in e.g. postgresql-simple 0.7.0.1, where it could produce the [libpq: another command is already in progress](https://github.com/haskellari/postgresql-simple/issues/69), which would hide the original exception.

And while that example might seem a bit farfetched, you will also run into this problem in postgresql-simple with `withTransaction`, since that tries to issue a `ROLLBACK` on exception. For example:

```haskell
withTransaction $
  runConcurrently $ (,) <$> Concurrently (readSomeFileFromDisk >> runSomeQuery) <*> Concurrently doSomethingElse
```

With Hpgsql you shouldn't run into such issues, or at least we promise that:
- Hpgsql is interruption-safe, so a query can be interrupted by asynchronous exceptions and you should still be able to run new queries on the same connection without any other side-effects. Naturally, it is up to you to determine which queries ran or not to completion, since they might have side-effects*.
- Hpgsql will throw either `PostgresError` or `IrrecoverableHpgsqlError`, and:
  - If you receive a `IrrecoverableHpgsqlError`, Hpgsql makes no promises about which statements ran to completion and what connection state is, and you should `closeForcefully` the connection without running any other queries. These errors should only be thrown for "obvious" developer mistakes from which usually there would be no way to proceed, anyway.
  - If you receive a `PostgresError` exception, postgres and Hpgsql's states are synced and you can issue new queries afterwards.
  - It is possible Hpgsql throws a different kind of exception. File a bug report if that happens, and if you know it came from Hpgsql, treat it like a `IrrecoverableHpgsqlError`.

* There are edge cases with different behaviour, but hpgsql will document them or throw an `IrrecoverableHpgsqlError` with informative instructions.
