## Working with this repository

Once you clone this repository, you will need [Nix](https://nixos.org/download/) and optionally [direnv](https://direnv.net/). Then you can run `direnv allow` or `nix-shell` in the project's root and you will have everything you need to build, run tests and benchmarks, locally.

I recommend you run `run list` to see what's available, but I'll paste a sample here as of 2026-04-22 as well:
```shell
$ run list
Commands:
  list            (builtin) List available commands
  help            (builtin) Show help for a command
  version         (builtin) Show run version
  benchmarks      Runs benchmarks with a postgresql DB listening.
  bench-single    Runs the benchmarks executable without all the CSV-producing and memory usage collecting tooling around it.
  ci-tests        Runs all tests that CI runs, exactly like CI runs them.
  format-hs       Formats all Haskell files with fourmolu.
  tests           Runs all tests.
  tests-stress    Runs tests 100 times, reporting how many passed and how many failed.
  tests-compat    Runs hpgsql-simple-compat's tests.
```

You don't need to install and configure PostgreSQL yourself: all the commands start temporary instances of PostgreSQL as necessary, and stop them at the end, including e.g. `run tests`.

But you can run `pg_ctl start` and then `psql postgres` to play with a local instance. Use `pg_ctl stop` to stop it.

For example:

```shell
$ run tests -- --match Pipelining # No need to start postgres as it happens automatically
```

## CI pipeline

Hpgsql's CI pipeline runs tests against all major supported versions of PostgreSQL, on Linux and Mac, and also tests the multi-threaded and single-threaded RTS.

It also runs hpgsql-simple-compat's (the API-compatible fork of postgresql-simple that uses Hpgsql internally) test suite, which is a slightly modified subset of postgresql-simple's own test suite.

My hope is that you can have a reasonable level of assurance that your contributions aren't breaking things.
