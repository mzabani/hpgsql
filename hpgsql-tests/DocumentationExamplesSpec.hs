{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tests the examples written in INTERRUPTION-SAFETY.md for both
-- hpgsql and postgresql-simple.
module DocumentationExamplesSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..), runConcurrently)
import Control.Exception.Safe (SomeException, bracket, fromException, onException, throwString, try)
import Control.Monad (void)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Database.PostgreSQL.Simple as PGSimple
import DbUtils (aroundConn, testConnInfo)
import Hpgsql
import Hpgsql.Connection (libpqConnString)
import Hpgsql.Query (sql)
import Test.Hspec

spec :: Spec
spec = describe "INTERRUPTION-SAFETY.md examples" $ do
  aroundConn $ do
    it
      "Hpgsql: flip onException logErrorToDatabase runs the handler and rethrows the original exception"
      onExceptionLogsErrorToDatabaseExample
  it
    "postgresql-simple: flip onException logErrorToDatabase fails with \"another command is already in progress\""
    postgresqlSimpleOnExceptionLogsErrorToDatabaseExample

-- | Reproduces the pseudo-code from INTERRUPTION-SAFETY.md:
--
-- > flip onException logErrorToDatabase $
-- >   runConcurrently $ (,)
-- >     <$> Concurrently (readSomeFileFromDisk >> runSomeQuery)
-- >     <*> Concurrently doSomethingElse
onExceptionLogsErrorToDatabaseExample :: HPgConnection -> IO ()
onExceptionLogsErrorToDatabaseExample conn = do
  handlerRanRef <- newIORef False

  let logErrorToDatabase :: IO ()
      logErrorToDatabase = do
        -- A trivial query is enough: the point is that we can successfully
        -- use the connection from the onException handler, right after a
        -- sibling query was interrupted by an async exception.
        execute conn [sql|SELECT 1|] `shouldReturn` 1
        writeIORef handlerRanRef True

      runSomeQuery :: IO ()
      runSomeQuery =
        -- A long-running server-side query, so we are guaranteed the
        -- async exception lands while postgres is still processing it.
        execute_ conn [sql|SELECT pg_sleep(5)|]

  result <-
    try @_ @SomeException $
      flip onException logErrorToDatabase $
        runConcurrently $
          (,)
            <$> Concurrently (readSomeFileFromDisk >> runSomeQuery)
            <*> Concurrently doSomethingElse

  case result of
    Right _ ->
      expectationFailure
        "Expected the concurrent block to throw, but it returned a value"
    Left e -> do
      -- The exception that escapes should be the original one from
      -- doSomethingElse, not anything introduced by the handler.
      show e `shouldContain` "doSomethingElse blew up"
      -- And it should not be an Hpgsql "irrecoverable" error just because
      -- we happened to run a query from the handler.
      case fromException @IrrecoverableHpgsqlError e of
        Just irrec ->
          expectationFailure $
            "Expected the original user exception to escape, but got an "
              <> "IrrecoverableHpgsqlError instead: "
              <> show irrec
        Nothing -> pure ()

  readIORef handlerRanRef
    `shouldReturn` True

  -- The connection must still be healthy afterwards.
  execute conn [sql|SELECT 1|] `shouldReturn` 1

-- | The exact same scenario as 'onExceptionLogsErrorToDatabaseExample',
-- but built directly on top of @postgresql-simple@ rather than Hpgsql.
postgresqlSimpleOnExceptionLogsErrorToDatabaseExample :: IO ()
postgresqlSimpleOnExceptionLogsErrorToDatabaseExample = do
  connInfo <- testConnInfo
  bracket
    (PGSimple.connectPostgreSQL (libpqConnString connInfo))
    PGSimple.close
    $ \conn -> do
      handlerResultRef <- newIORef Nothing

      let logErrorToDatabase :: IO ()
          logErrorToDatabase = do
            -- Capture whatever happens when the handler tries to use the
            -- connection. We deliberately do not let this exception
            -- escape the handler: we want to observe it, regardless of
            -- how the surrounding `onException` chooses to surface it.
            r <- try @_ @SomeException $ void $ PGSimple.execute_ conn "SELECT 1"
            writeIORef handlerResultRef (Just r)

          runSomeQuery :: IO ()
          runSomeQuery =
            void $ PGSimple.execute_ conn "SELECT pg_sleep(5)"

      _ <-
        try @_ @SomeException $
          flip onException logErrorToDatabase $
            runConcurrently $
              (,)
                <$> Concurrently (readSomeFileFromDisk >> runSomeQuery)
                <*> Concurrently doSomethingElse

      handlerResult <- readIORef handlerResultRef
      case handlerResult of
        Nothing ->
          expectationFailure
            "Expected the onException handler to have run, but it did not"
        Just (Right _) ->
          expectationFailure $
            "Expected postgresql-simple's logErrorToDatabase query to fail "
              <> "with \"another command is already in progress\", but it "
              <> "succeeded."
        Just (Left handlerExc) ->
          show handlerExc `shouldContain` "another command is already in progress"

readSomeFileFromDisk :: IO ()
readSomeFileFromDisk =
  threadDelay 50_000

doSomethingElse :: IO ()
doSomethingElse = do
  threadDelay 250_000
  throwString "doSomethingElse blew up"
