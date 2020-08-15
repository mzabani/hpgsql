TODO: Check this article is correct, particularly its claims about postgresql-simple and Hpgsql's exceptions.
Before I start, let me say the issues I'm about to show are uncommon and as far as I've seen not consequential in a bad way in practice.

Disclaimer apart, imagine you have concurrent code like this:
```haskell
runConcurrently $ (,) <$> Concurrently readSomeFileFromDisk <*> Concurrently doSomethingCpuIntensive
```

What would happen if in either of the concurrent threads you added a database query? 

Let's say this is your code now (in pseudo-code-ish Haskell):
```haskell
flip onException logErrorToDatabase $
  runConcurrently $ (,) <$> Concurrently (readSomeFileFromDisk >> runSomeQuery) <*> Concurrently doSomethingCpuIntensive
```

This would be fine in most runs, but it could lead to the [libpq: another command is already in progress](https://github.com/haskellari/postgresql-simple/issues/69) error in case one of the threads throws an exception, like so:
1 - `doSomethingCpuIntensive` throws an exception
2 - `runConcurrently` sees the exception and throws an asynchronous exception to the thread reading from disk and running a query.
3 - That asynchronous exception kills the thread after `runSomeQuery` had sent the query to postgres, but before the query finished running.
4 - `runConcurrently` re-throws the original exception, and our `onException` handler tries to run a query. But libpq only accepts one query at a time, and far as postgres is concerned, it might still be running the query from `runSomeQuery`. With postgresql-simple, you would run into the github issue linked above.

And even if the query would not have erred itself, you wouldn't log the error to your database if this happens, and the original exception might be hidden by the new "libpq: another command is already in progress" exception.

You might also run into that problem in postgresql-simple with `withTransaction`, since that tries to issue a `ROLLBACK` on exception. And this might be really tricky:

```haskell
withTransaction $
  runConcurrently $ (,) <$> Concurrently (readSomeFileFromDisk >> runSomeQuery) <*> Concurrently doSomethingCpuIntensive
```

If you want a guarantee that the transaction state is either committed or rolled back after a `withTransaction` block, it
might actually leave the transaction in a different state, and the exception might not help determine the source of the problem.

With Hpgsql you shouldn't run into such issues, or at least we promise that:
- Hpgsql is interruption safe, so a query can be interrupted by asynchronous exceptions and you should still be able to run new queries on the same connection. Naturally, it is up to you to determine which queries ran or not to completion, since they might have side-effects.
- HPgsql will throw either `PostgresError` or `IrrecoverableHpgsqlError`, and:
  - If you receive a `IrrecoverableHpgsqlError`, Hpgsql makes no promises about which statements ran to completion and what connection state is, and you should `closeForcefully` the connection without running any other queries.
  - If you receive a `PostgresError` exception, postgres and Hpgsql's states are synced and you can issue new queries afterwards.
  - It is possible HPgsql throws a different kind of exception. File a bug report if that happens, and if you know it came from HPgsql, treat it like a `IrrecoverableHpgsqlError`.
