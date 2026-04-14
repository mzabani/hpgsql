{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ThreadSafetySpec where

import Control.Concurrent (modifyMVar_, myThreadId, threadDelay)
import Control.Concurrent.Async (Concurrently (..), cancel, forConcurrently, forConcurrently_, mapConcurrently, wait, withAsync)
import Control.Exception.Safe (SomeException, try)
import Control.Monad (forM_, void, when)
import Data.Containers.ListUtils (nubOrd)
import Data.Text (Text)
import DbUtils
  ( aroundConn,
    irrecoverableErrorWithMsg,
    pgErrorMustContain,
    testConnInfo,
    withRollback,
  )
import HPgsql
import HPgsql.Query (sql)
import Streaming (Of (..))
import qualified Streaming.Prelude as S
import System.Mem (performGC)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
  describe "Thread safety, interruption safety and trickier error semantics" $ do
    parallel $ do
      it
        "Query cancellation in the future"
        queryCancellationInTheFuture
      aroundConn $ do
        it
          "Exercise interruption safety"
          exerciseInterruptionSafety

        it
          "Query interruption is semantics-preserving inside explicit transaction"
          interruptingQueryInsideTransactionPreservesSemantics
        it
          "Send queries concurrently"
          sendQueriesConcurrently
        it
          "Cancel active streaming query then try to consume its results"
          cancelStreamingQueryThenTryToConsumeResults
        it
          "Thread that sends pipeline must be the thread that consumes results"
          threadThatSendsPipelinMustBeThreadThatConsumesResults
        forM_ [False, True] $ \cancelQueryExplicitly -> do
          it
            ("Query that errors due to bad FromPgField implementation - " ++ show cancelQueryExplicitly)
            (queryThatErrorsDueToBadFromPgFieldImplementation1 cancelQueryExplicitly)
          it
            ("Query that errors due to bad FromPgField implementation - streaming - " ++ show cancelQueryExplicitly)
            ( queryThatErrorsDueToBadFromPgFieldImplementation2
                cancelQueryExplicitly
            )
        forM_ [False, True] $ \useTimeout -> do
          it
            ("Can send query after thread killed - useTimeout " ++ show useTimeout)
            (sendQueryAfterThreadKilled useTimeout)
        it
          "Assumptions about ThreadId behaviour"
          assumptionsAboutThreadIdBehaviour
        it
          "cancelAnyRunningStatement does not require any queries to be running and is idempotent"
          cancelAnyRunningStatementIsIdempotent

    -- Tests below cannot run in parallel to itself for whatever reasons
    aroundConn $ do
      forM_ [False, True] $ \useTimeout -> do
        it
          ("Can send query after COPY thread killed - useTimeout " ++ show useTimeout)
          (sendQueryAfterCopyKilled useTimeout)

-- | A function that behaves just like `timeout` except that `myThreadId` inside the time-limited
-- action will be the same as the calling thread's if `useTimeout` is true (because it will use `timeout`)
-- or will be different because it will emulate `timeout` through `withAsync`.
getQueryKilledBeforeItsFinished :: Bool -> Int -> IO a -> IO (Maybe a)
getQueryKilledBeforeItsFinished useTimeout waitTime f =
  if useTimeout
    then timeout waitTime f
    else do
      ex <- try $ withAsync f (\forkedAction -> threadDelay waitTime >> error "interrupt everything" >> wait forkedAction)
      case ex of
        Left (_ :: SomeException) -> pure Nothing
        Right v -> pure $ Just v

sendQueryAfterThreadKilled :: Bool -> HPgConnection -> IO ()
sendQueryAfterThreadKilled useTimeout conn = do
  -- Give enough time to be certain the query was sent to postgres
  -- See Note [`timeout` uses the same ThreadId] for why we need to test both
  -- `timeout` and `withAsync`
  let firstQuery = queryWithStreaming (rowParser @(Only ())) conn [sql|SELECT x, pg_sleep(x / 1000.0) FROM generate_series(1,1000) q(x)|]
  didNotFinish <- emulatedTimeout 300_000 $ firstQuery >>= S.effects
  didNotFinish `shouldBe` Nothing
  didNotFinish2 <- emulatedTimeout 300_000 $ queryWith (rowParser @(Only ())) conn [sql|SELECT pg_sleep(999)|]
  didNotFinish2 `shouldBe` Nothing
  didNotFinish3 <- emulatedTimeout 300_000 $ runPipeline conn (traverse pipelineCmd [[sql|SELECT pg_sleep(5)|], [sql|SELECT 37|]]) >>= sequenceA
  case didNotFinish3 of
    Just _ -> expectationFailure "didNotFinish3 was supposed to have been killed before finishing"
    Nothing -> pure ()
  didNotFinish4 <- emulatedTimeout 300_000 $ do
    (firstCmd, secondCmd) <- runPipeline conn $ (,) <$> pipelineL (rowParser @(Only ())) [sql|SELECT pg_sleep(0.3) -- About the same amount as the emulatedTimeout|] <*> pipelineCmd [sql|SELECT pg_sleep(5)|]
    void firstCmd
    secondCmd
  case didNotFinish4 of
    Just _ -> expectationFailure "didNotFinish4 was supposed to have been killed before finishing"
    Nothing -> pure ()
  queryWith (rowParser @(Only Int)) conn [sql|with nums(v) as (values (1), (2), (3)) SELECT v FROM nums|]
    `shouldReturn` [Only 1, Only 2, Only 3]
  where
    emulatedTimeout :: Int -> IO a -> IO (Maybe a)
    emulatedTimeout = getQueryKilledBeforeItsFinished useTimeout

sendQueryAfterCopyKilled :: Bool -> HPgConnection -> IO ()
sendQueryAfterCopyKilled useTimeout conn = do
  execute_ conn [sql|CREATE TABLE employee (    employee_id SERIAL PRIMARY KEY    , employee_name TEXT NOT NULL);|]
  -- Give enough time to be certain the query was sent to postgres
  didNotFinish <- getQueryKilledBeforeItsFinished useTimeout 300_000
    $ withCopy
      conn
      [sql|COPY employee FROM STDIN WITH (FORMAT CSV);|]
    $ do
      putCopyData conn "5,Dracula\n"
      putCopyData conn "6,The Grinch\n"
      threadDelay 10_000_000
  didNotFinish `shouldBe` Nothing
  queryWith (rowParser @(Int, Text)) conn [sql|SELECT * FROM employee|] `shouldReturn` []
  execute_ conn [sql|DROP TABLE employee|]

assumptionsAboutThreadIdBehaviour :: HPgConnection -> IO ()
assumptionsAboutThreadIdBehaviour _conn = do
  thisThreadId <- myThreadId
  thousandThreadIds <- mapConcurrently (const myThreadId) [1 .. 1000]
  thousandThreadIds `shouldNotContain` [thisThreadId]
  map show thousandThreadIds `shouldNotContain` [show thisThreadId]
  -- Stronger check: all of them have distinct Ids, no reuse
  length (nubOrd (map show thousandThreadIds)) `shouldBe` 1000
  -- Give time for threads to be collected, and don't be stingy
  threadDelay 50_000
  performGC
  threadDelay 50_000
  performGC
  -- Repeat all tests, and ensure all threadids are new yet again
  thousandThreadIds2 <- mapConcurrently (const myThreadId) [1 .. 1000]
  thousandThreadIds2 `shouldNotContain` [thisThreadId]
  map show thousandThreadIds2 `shouldNotContain` [show thisThreadId]
  length (nubOrd $ map show thousandThreadIds ++ map show thousandThreadIds2) `shouldBe` 2000

cancelAnyRunningStatementIsIdempotent :: HPgConnection -> IO ()
cancelAnyRunningStatementIsIdempotent conn = do
  cancelAnyRunningStatement conn
  cancelAnyRunningStatement conn
  cancelAnyRunningStatement conn
  cancelAnyRunningStatement conn

sendQueriesConcurrently :: HPgConnection -> IO ()
sendQueriesConcurrently conn = forConcurrently_ [1 .. 10] $ const $ do
  (res1, res2, res3) <-
    runConcurrently $
      (,,)
        <$> Concurrently (queryWith (rowParser @(Int, Int, ())) conn "with nums(v) as (values (37), (49), (-13)) SELECT v, 10, pg_sleep(0.1) FROM nums")
        <*> Concurrently (queryWith (rowParser @(Int, Int, ())) conn "with nums(v) as (values (1), (2), (3)) SELECT v, 11, pg_sleep(0.1) FROM nums")
        <*> Concurrently (queryWith (rowParser @(Int, Int, ())) conn "with nums(v) as (values (4), (5), (6)) SELECT v, 12, pg_sleep(0.1) FROM nums")
  res1 `shouldBe` [(37, 10, ()), (49, 10, ()), (-13, 10, ())]
  res2 `shouldBe` [(1, 11, ()), (2, 11, ()), (3, 11, ())]
  res3 `shouldBe` [(4, 12, ()), (5, 12, ()), (6, 12, ())]

cancelStreamingQueryThenTryToConsumeResults :: HPgConnection -> IO ()
cancelStreamingQueryThenTryToConsumeResults conn = do
  res <- queryWithStreaming (rowParser @(Only Int)) conn "SELECT * FROM generate_series(1,100000000)"
  firstTwoElems <- S.toList_ $ S.take 2 res
  thirdAndFourthElems :> restOfStream <- S.toList $ S.splitAt 2 res
  fifthAndSixthElems <- S.toList_ $ S.take 2 restOfStream
  firstTwoElems `shouldBe` [Only 1, Only 2]
  thirdAndFourthElems `shouldBe` [Only 3, Only 4]
  fifthAndSixthElems `shouldBe` [Only 5, Only 6]
  cancelAnyRunningStatement conn
  -- Consuming the stream now will do what? For now it:
  -- 1. Might not throw if the query finished running in the server. Change 100000000 to 5 to see this test fail.
  -- 2. Might throw with "canceling statement due to user request", like in this test.
  S.effects res `shouldThrow` pgErrorMustContain "generate_series(1,100000000)" [(ErrorCode, "57014")]

threadThatSendsPipelinMustBeThreadThatConsumesResults :: HPgConnection -> IO ()
threadThatSendsPipelinMustBeThreadThatConsumesResults conn = do
  res <- queryWithStreaming (rowParser @(Only Int)) conn "SELECT * FROM generate_series(1,10)"
  withAsync (S.effects res) $ \otherThreadResults ->
    wait otherThreadResults `shouldThrow` irrecoverableErrorWithMsg "HPgsql does not support consuming different SQL statements' results of the same pipeline from different threads. Behaviour is undefined if you try that."

queryCancellationInTheFuture :: IO ()
queryCancellationInTheFuture = do
  hpgsqlConnInfo <- testConnInfo
  -- This needs to run _many_ times for us to be certain
  -- because it's a race condition.
  -- Follow the cancellation code path in `internalConnectOrCancel`
  -- and comment out the waiting-for-socket-closed part to see
  -- this test fail with a query cancelled exception, at least some
  -- times.
  forM_ [1 :: Int .. 1_000] $
    const $
      withConnection hpgsqlConnInfo 10 $ \conn -> do
        cancelAnyRunningStatement conn
        execute conn "select true" `shouldReturn` 1

-- | Massively exercise sending an asynchronous exception to a query
-- that is actively receiving data from the backend (so not abusing
-- pg_sleep). The idea is to try to interrupt hpgsql in the middle
-- of receiving messages, since taking from the kernel's buffers
-- is a blocking and risky process.
exerciseInterruptionSafety :: HPgConnection -> IO ()
exerciseInterruptionSafety conn = do
  let veryLongTxtString :: String = take 10000 $ repeat 'x'
  forM_ [0_001 :: Int .. 2_000] $ \i -> do
    withAsync (execute conn [sql|select #{veryLongTxtString}, pg_sleep(0.1) FROM generate_series(1, 100000)|]) $ \queryAsync -> do
      threadDelay i
      -- putStrLn $ "Killing thread, time: " ++ show i
      cancel queryAsync
    execute conn "SELECT 1" `shouldReturn` 1

-- | Inside an explicit transaction, cancelling an interrupted query would
-- make the entire transaction fail because of the error. We want interruption
-- not to change connection state (beyond what running or not running user queries
-- might already, of course), so the explicit transaction needs to be healthy after
-- query interruption.
interruptingQueryInsideTransactionPreservesSemantics :: HPgConnection -> IO ()
interruptingQueryInsideTransactionPreservesSemantics conn = withRollback conn $ do
  forM_ [1 .. 2] $ const $ do
    withAsync (execute conn [sql|select pg_sleep(0.5)|]) $ \queryAsync -> do
      threadDelay 250_000
      cancel queryAsync
    -- The timeouts above pretty much ensure the query started and was cancelled
    -- during its execution. If the transaction is unhealthy, the next query would
    -- throw an error with "current transaction is aborted, commands ignored .."
    transactionStatus conn `shouldReturn` TransActive
    execute conn "SELECT 1" `shouldReturn` 1

queryThatErrorsDueToBadFromPgFieldImplementation1 :: Bool -> HPgConnection -> IO ()
queryThatErrorsDueToBadFromPgFieldImplementation1 cancelQueryExplicitly conn = do
  tid <- myThreadId
  queryWith (rowParser @(Only Bool)) conn "SELECT CASE WHEN x >= 300 THEN NULL::boolean ELSE TRUE END FROM generate_series(1,100000) q(x)"
    `shouldThrow` irrecoverableErrorWithMsg "Failed parsing a row"

  -- We have automatic cancellation when the same thread (see Note [`timeout` uses the same ThreadId]) tries to run
  -- a query before finishing to consume the results of an earlier query,
  -- but we don't promise users they can do this after an IrrecoverableHpgsqlError.
  when cancelQueryExplicitly $ cancelAnyRunningStatement conn
  execute conn "select true" `shouldReturn` 1
  queryWith rowParser conn "select 37" `shouldReturn` [Only (37 :: Int)]

queryThatErrorsDueToBadFromPgFieldImplementation2 :: Bool -> HPgConnection -> IO ()
queryThatErrorsDueToBadFromPgFieldImplementation2 cancelQueryExplicitly conn = do
  -- A streaming variant that only throws in later elements of the stream, since that can render internal and naive exception-catching moot
  res <- queryWithStreaming (rowParser @(Only Bool)) conn "SELECT CASE WHEN x >= 300 THEN NULL::boolean ELSE TRUE END FROM generate_series(1,10000000) q(x)"
  S.effects res `shouldThrow` irrecoverableErrorWithMsg "Failed parsing a row"
  -- We have automatic cancellation when the same thread (see Note [`timeout` uses the same ThreadId]) tries to run
  -- a query before finishing to consume the results of an earlier query,
  -- but we don't promise users they can do this after an IrrecoverableHpgsqlError.
  when cancelQueryExplicitly $ cancelAnyRunningStatement conn
  -- modifyMVar_ _globalDebugLock $ const (pure True)
  -- putStrLn "AAAAAAAAAAAAA"
  execute conn "select true" `shouldReturn` 1
  -- modifyMVar_ _globalDebugLock $ const (pure False)
  -- putStrLn "BBBBBBBBBBBB"
  queryWith rowParser conn "select 37" `shouldReturn` [Only (37 :: Int)]

-- Note [`timeout` uses the same ThreadId]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- I was under the impression that `timeout` forks a different thread, but
-- it might not, at least under the concurrent RTS, as per official docs.
-- This means interruption safety requires us to cancel and drain active
-- queries even when they were issued from the same ThreadId.
