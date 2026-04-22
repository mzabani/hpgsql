{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tests the examples written in INTERRUPTION-SAFETY.md for both
-- hpgsql and postgresql-simple.
module DocumentationExamplesSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..), runConcurrently)
import Control.Exception.Safe (SomeException, bracket, fromException, onException, throwString, try, tryAny)
import Control.Monad (forM_, void)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.List as List
import qualified Database.PostgreSQL.Simple as PGSimple
import DbUtils (aroundConn, testConnInfo)
import Hpgsql
import Hpgsql.Connection (libpqConnString)
import Hpgsql.Query (sql)
import Hpgsql.Transaction (transactionStatus, withTransaction)
import Test.Hspec

spec :: Spec
spec = describe "INTERRUPTION-SAFETY.md examples" $ parallel $ do
  aroundConn $ do
    it
      "Hpgsql: try + logErrorToDatabase runs the handler and rethrows the original exception"
      onExceptionLogsErrorToDatabaseExample
    it
      "Hpgsql: withTransaction rolls back cleanly when an async exception interrupts a query"
      withTransactionExample
  it
    "postgresql-simple: try + logErrorToDatabase fails with \"another command is already in progress\""
    postgresqlSimpleOnExceptionLogsErrorToDatabaseExample
  it
    "postgresql-simple: withTransaction leaves the connection in a bad state (\"another command is already in progress\")"
    postgresqlSimpleWithTransactionExample

onExceptionLogsErrorToDatabaseExample :: HPgConnection -> IO ()
onExceptionLogsErrorToDatabaseExample conn = do
  let logErrorToDatabase :: IO ()
      logErrorToDatabase =
        execute conn "SELECT 1" `shouldReturn` 1

      runSomeQuery :: IO ()
      runSomeQuery =
        -- A long-running server-side query, so we are guaranteed the
        -- async exception lands while postgres is still processing it.
        execute_ conn [sql|SELECT pg_sleep(0.3)|]

  -- If you change this test to `flip onException logErrorToDatabase` it
  -- fails very reliably in CI (but not locally) with "blocked indefinitely on mvar".
  -- It's not clear why, but running a query inside `onException` is running it
  -- with async exceptions masked, which is very much not recommended.
  -- Still, it would be good to figure out why this happens one day.
  ex <-
    tryAny $
      runConcurrently $
        (,)
          <$> Concurrently (readSomeFileFromDisk >> runSomeQuery)
          <*> Concurrently doSomethingElse
  case ex of
    Right _ -> expectationFailure "Expected an exception"
    Left e -> do
      e
        `shouldSatisfy` ( \(ex :: SomeException) ->
                            "doSomethingElse blew up" `List.isInfixOf` show ex
                        )
      logErrorToDatabase -- Should not throw

-- | The exact same scenario as 'onExceptionLogsErrorToDatabaseExample',
-- but built directly on top of @postgresql-simple@ rather than Hpgsql.
postgresqlSimpleOnExceptionLogsErrorToDatabaseExample :: IO ()
postgresqlSimpleOnExceptionLogsErrorToDatabaseExample = do
  connInfo <- testConnInfo
  bracket
    (PGSimple.connectPostgreSQL (libpqConnString connInfo))
    PGSimple.close
    $ \conn -> do
      let logErrorToDatabase :: IO ()
          logErrorToDatabase = do
            void $ PGSimple.execute_ conn "SELECT 1"

          runSomeQuery :: IO ()
          runSomeQuery =
            void $ PGSimple.execute_ conn "SELECT pg_sleep(0.3)"

      ex <-
        tryAny $
          runConcurrently $
            (,)
              <$> Concurrently (readSomeFileFromDisk >> runSomeQuery)
              <*> Concurrently doSomethingElse

      case ex of
        Right _ -> expectationFailure "Did not get an exception"
        Left _ -> logErrorToDatabase `shouldThrow` (\(ex :: SomeException) -> "another command is already in progress" `List.isInfixOf` show ex)

withTransactionExample :: HPgConnection -> IO ()
withTransactionExample conn = do
  let runSomeQuery :: IO ()
      runSomeQuery =
        execute_ conn [sql|SELECT pg_sleep(0.3)|]

  ( withTransaction conn $ do
      transactionStatus conn `shouldReturn` TransInTrans
      runConcurrently $
        (,)
          <$> Concurrently (readSomeFileFromDisk >> runSomeQuery)
          <*> Concurrently doSomethingElse
    )
    `shouldThrow` ( \(ex :: SomeException) ->
                      "doSomethingElse blew up" `List.isInfixOf` show ex
                  )

  -- Run any other query and verify the transaction was rolled back
  execute_ conn "SELECT 1"
  transactionStatus conn `shouldReturn` TransIdle

-- | The exact same scenario as 'withTransactionExample', but for postgresql-simple.
postgresqlSimpleWithTransactionExample :: IO ()
postgresqlSimpleWithTransactionExample = do
  connInfo <- testConnInfo
  bracket
    (PGSimple.connectPostgreSQL (libpqConnString connInfo))
    PGSimple.close
    $ \conn -> do
      let runSomeQuery :: IO ()
          runSomeQuery =
            void $ PGSimple.execute_ conn "SELECT pg_sleep(0.3)"

      void $
        tryAny $
          PGSimple.withTransaction conn $
            runConcurrently $
              (,)
                <$> Concurrently (readSomeFileFromDisk >> runSomeQuery)
                <*> Concurrently doSomethingElse

      -- New queries will throw
      PGSimple.execute_ conn "SELECT 1" `shouldThrow` \(ex :: SomeException) -> "another command is already in progress" `List.isInfixOf` show ex

readSomeFileFromDisk :: IO ()
readSomeFileFromDisk =
  threadDelay 10_000

doSomethingElse :: IO ()
doSomethingElse = do
  threadDelay 100_000
  throwString "doSomethingElse blew up"
