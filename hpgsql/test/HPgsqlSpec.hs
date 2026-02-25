{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module HPgsqlSpec where

import Control.Concurrent (myThreadId, threadDelay)
import Control.Concurrent.Async (Concurrently (..), mapConcurrently, wait, withAsync)
import Control.Exception.Safe (SomeException, catch, throw, try)
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Containers.ListUtils (nubOrd)
import Data.Functor ((<&>))
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as Text
import Data.Time (Day, DiffTime, NominalDiffTime, UTCTime (..), fromGregorian, picosecondsToDiffTime, secondsToDiffTime)
import Data.Time.LocalTime (CalendarDiffTime (..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import DbUtils
  ( aroundConn,
    testConnInfo,
  )
import Debug.Trace
import GHC.Generics (Generic)
import HPgsql
import HPgsql.Field (AllowNull (..), LowerCasedPgEnum (..), ToPgField (..), anyTypeDecoder, compositeTypeParser, singleColRowParser)
import HPgsql.Query (Query (..), SingleQuery (..), sql)
import HPgsql.TypeInfo (Oid)
import Hedgehog (PropertyT, (===))
import qualified Hedgehog as Gen
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Gen
import Streaming (Of (..))
import qualified Streaming.Prelude as S
import System.Mem (performGC)
import System.Timeout (timeout)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
  aroundConn $ describe "HPgsql" $ do
    it
      "Querying and returning a few rows"
      queryingAndReturningAFewRows
    it
      "Querying and returning a few rows with more than one statement in the same query"
      queryingAndReturningAFewRowsMoreThanOneStatement
    it
      "Querying and returning 0 rows"
      queryingAndReturningZeroRows
    it
      "Querying streaming sum"
      queryingStreamingSum
    it
      "Executing non-rows-returning statement"
      executingCountReturningStatements
    it
      "Executing query string with multiple statements and the last one being a non-rows-returning statement"
      executingMixedRowsAndCountReturningStatements
    it
      "Mismatch in number of columns"
      queryWithMismatchInNumberOfColumns
    it
      "Mismatch in column types"
      queryWithMismatchInTypesOfColumns
    it
      "Query that errors and another successful query after that"
      queryThatErrorsFollowedBySuccessfulQuery
    it
      "Query that errors before returning any rows and another successful query after that"
      queryThatErrorsBeforeReturningAnyRowsFollowedBySuccessfulQuery
    it
      "Applying commands without caring for returned rows/value"
      discardingReturnCountsAndValues
    it
      "Prepared statement - well formed queries"
      wellFormedPreparedStatements
    it
      "Values round-trip"
      valuesRoundTrip
    it
      "bytea values round-trip"
      byteaValuesRoundTrip

    it
      "Date types round-trip"
      dateAndTimestampTzRoundTrip
    it
      "Numeric values round-trip"
      numericValuesRoundTrip
    it
      "Numeric values round-trip even when result types are larger"
      numericValuesRoundTripEvenWhenTargetTypesAreLarger

    it
      "Date decoding"
      dateDecoding
    it
      "Date encoding"
      dateEncoding
    it
      "Timestamp decoding"
      timestampDecoding
    it
      "Timestamp encoding"
      timestampEncoding
    it
      "Less usual types"
      lessUsualTypes
    it
      "Prepared statement - bad result row parser - right number of columns but wrong types throws"
      preparedStatementWrongTypes
    it
      "Prepared statement - bad result row parser - right number of columns but wrong types throws even without returned rows"
      preparedStatementWrongTypesThrowsEvenWithZeroReturnedRows
    it
      "Command counts"
      checkCommandCounts
    it
      "Query transaction status in all transaction states"
      queryTransactionStatusInAllTransactionStates
    it
      "Raise NOTICE then run query"
      raiseNoticeThenRunQuery
    it
      "Changing parameter statuses"
      changeParameterStatus
  aroundConn $ describe "Pipelining" $ do
    it
      "runPipeline with multiple statements"
      runPipelineWithMultipleStatements
    it
      "runPipeline error semantics - postgres error"
      runPipelineErrorSemantics
    it
      "runPipeline error semantics - user error"
      runPipelineErrorSemanticsUnsupportedCaseStillBehavesWell
    it
      "runPipeline runs statements in an implicit transaction"
      runPipelineRunsInImplicitTransaction
  aroundConn $ describe "Custom types" $ do
    it "Composite type" queryCompositeType
    it
      "Querying array types"
      queryArrayTypes
    it
      "Querying enum types"
      queryEnumTypes
    it
      "Generically derived types"
      queryGenericallyDerivedTypes
  aroundConn $ describe "Notifications" $ do
    it
      "Send notifications and then receive them"
      sendNotifAndReceiveIt

    it
      "Mixing order of sending and receiving notifications"
      mixedOrderSendAndReceiveNotifications

    -- Read the comment inside the test below to understand why
    -- it deadlocks. I don't think we can do something about this
    -- unless we can detect one thread is blocked on the other.
    -- We probably shouldn't try anything of the sort, though.
    xit
      "getNotification blocks and works even after orphaned query"
      getNotificationBlocksAndWorksEvenAfterOrphanedQuery
  aroundConn $ describe "beforeReturningToPool" $ do
    it
      "Pending notification is cleared by beforeReturningToPool"
      pendingNotifClearedByBeforeReturningToPool
    it
      "Bad transaction state means exception"
      badTransactionStateMeansException
  aroundConn $ describe "COPY" $ do
    it
      "COPY succeeding"
      copyStatementSucceeding
    it
      "COPY error"
      copyError
  describe "SQL Quasiquoter" $ do
    it "parses SQL without interpolation" $ do
      let Query queries = [sql|SELECT * FROM users|]
          query = getUniqueNE queries
      queryString query `shouldBe` encodeUtf8 "SELECT * FROM users"
      queryParams query `shouldBe` []

    it "parses SQL with single interpolation" $ do
      let userId = 42 :: Int
          Query queries = [sql|SELECT * FROM users WHERE id = #{userId}|]
          query = getUniqueNE queries
      queryString query `shouldBe` encodeUtf8 "SELECT * FROM users WHERE id = $1"
      length (queryParams query) `shouldBe` 1

    it "parses SQL with multiple interpolations" $ do
      let userId = 42 :: Int
          userName = "john" :: Text
          Query queries = [sql|SELECT * FROM users WHERE id = #{userId} AND name = #{userName}|]
          query = getUniqueNE queries
      queryString query `shouldBe` encodeUtf8 "SELECT * FROM users WHERE id = $1 AND name = $2"
      length (queryParams query) `shouldBe` 2
  aroundConn $ describe "Thread safety and trickier error semantics" $ do
    it
      "Send queries concurrently"
      sendQueriesConcurrently
    it
      "Cancel active streaming query then try to consume its results"
      cancelStreamingQueryThenTryToConsumeResults
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
        ("Can send query after COPY thread killed - useTimeout " ++ show useTimeout)
        (sendQueryAfterCopyKilled useTimeout)
    it
      "Assumptions about ThreadId behaviour"
      assumptionsAboutThreadIdBehaviour
    it
      "cancelAnyRunningStatement does not require any queries to be running and is idempotent"
      cancelAnyRunningStatementIsIdempotent
  describe "Connectivity" $ do
    it
      "Connecting to non-existing db"
      connectingToNonExistingDb
    it
      "Connecting with connect-time options"
      connectingWithConnectTimeOptions

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
sendQueriesConcurrently conn = do
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
  -- So we should document the behaviour when doing this as UNDEFINED!
  S.effects res `shouldThrow` pgErrorMustContain [(ErrorCode, "57014")]

queryingAndReturningAFewRows :: HPgConnection -> IO ()
queryingAndReturningAFewRows conn =
  forM_ [1 .. 2] $
    const $
      queryWith (rowParser @(Int, Int)) conn "with nums(v) as (values (37), (49), (-13)) SELECT v, 10 FROM nums" `shouldReturn` [(37, 10), (49, 10), (-13, 10)]

queryingAndReturningAFewRowsMoreThanOneStatement :: HPgConnection -> IO ()
queryingAndReturningAFewRowsMoreThanOneStatement conn = do
  queryWith (rowParser @(Int, Int)) conn "SELECT 1; SELECT TRUE; with nums(v) as (values (37), (49), (-13)) SELECT v, 10 FROM nums" `shouldReturn` [(37, 10), (49, 10), (-13, 10)]

queryingAndReturningZeroRows :: HPgConnection -> IO ()
queryingAndReturningZeroRows conn =
  forM_ [1 .. 2] $
    const $
      queryWith (rowParser @(Only Int)) conn "SELECT * FROM generate_series(1,3) WHERE false" `shouldReturn` []

queryingStreamingSum :: HPgConnection -> IO ()
queryingStreamingSum conn = do
  res <- queryWithStreaming (rowParser @(Only Int)) conn "SELECT * FROM generate_series(1,10000)"
  S.sum_ (S.map fromOnly res) `shouldReturn` ((1 + 10000) * 5000)

executingCountReturningStatements :: HPgConnection -> IO ()
executingCountReturningStatements conn = do
  void $ execute conn "CREATE TABLE xyz();"
  void $ execute conn "DROP TABLE xyz;"

executingMixedRowsAndCountReturningStatements :: HPgConnection -> IO ()
executingMixedRowsAndCountReturningStatements conn = do
  executeMany conn ["SELECT 1", "CREATE TABLE xyz();", "DELETE FROM xyz;"] `shouldReturn` [1, 0, 0]
  execute_ conn "DROP TABLE xyz;"

queryWithMismatchInNumberOfColumns :: HPgConnection -> IO ()
queryWithMismatchInNumberOfColumns conn = do
  queryWith (rowParser @(Bool, Bool)) conn "select 1, 2, 3"
    `shouldThrow` pgErrorMustContain [(ErrorCode, "08P01"), (ErrorHumanReadableMsg, "bind message has 2 result formats but query has 3 columns")]

queryWithMismatchInTypesOfColumns :: HPgConnection -> IO ()
queryWithMismatchInTypesOfColumns conn = do
  queryWith (rowParser @(Bool, Bool)) conn "select 1, 2"
    `shouldThrow` irrecoverableErrorWithMsg "Query result column types do not match expected column types"

queryThatErrorsFollowedBySuccessfulQuery :: HPgConnection -> IO ()
queryThatErrorsFollowedBySuccessfulQuery conn = do
  -- TODO: Test empty query string
  -- TODO: Test multiple statements inside the same query string
  -- TODO: Test error statement + empty statement + normal statement all inside the same query string
  queryWith (rowParser @(Only Bool)) conn "select 1/(x - 2) > 0 from generate_series(1,2) subq(x)"
    `shouldThrow` pgErrorMustContain [(ErrorCode, "22012"), (ErrorHumanReadableMsg, "division by zero")]
  queryWith (rowParser @(Only Bool)) conn "select FALSE from generate_series(1,2) subq(x)"
    `shouldReturn` [Only False, Only False]

queryThatErrorsBeforeReturningAnyRowsFollowedBySuccessfulQuery :: HPgConnection -> IO ()
queryThatErrorsBeforeReturningAnyRowsFollowedBySuccessfulQuery conn = do
  queryWith (rowParser @(Only Bool)) conn "select 1/0"
    `shouldThrow` pgErrorMustContain [(ErrorCode, "22012"), (ErrorHumanReadableMsg, "division by zero")]
  queryWith (rowParser @(Only Bool)) conn "select FALSE from generate_series(1,2) subq(x)"
    `shouldReturn` [Only False, Only False]

queryThatErrorsDueToBadFromPgFieldImplementation1 :: Bool -> HPgConnection -> IO ()
queryThatErrorsDueToBadFromPgFieldImplementation1 cancelQueryExplicitly conn = do
  tid <- myThreadId
  queryWith (rowParser @(Only Bool)) conn "SELECT CASE WHEN x >= 300 THEN NULL::boolean ELSE TRUE END FROM generate_series(1,100000) q(x)"
    `shouldThrow` irrecoverableErrorWithMsg "Failed parsing a row"

  -- We have automatic cancellation when the same thread (see Note [`timeout` uses the same ThreadId]) tries to run
  -- a query before finishing to consume the results of an earlier query,
  -- but we don't promise users they can do this after an IrrecoverableHpgsqlError.
  when cancelQueryExplicitly $ liftIO $ cancelAnyRunningStatement conn
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
  when cancelQueryExplicitly $ liftIO $ cancelAnyRunningStatement conn
  execute conn "select true" `shouldReturn` 1
  queryWith rowParser conn "select 37" `shouldReturn` [Only (37 :: Int)]

discardingReturnCountsAndValues :: HPgConnection -> IO ()
discardingReturnCountsAndValues conn = withRollback conn $ do
  executeMany_ conn ["CREATE TABLE xx(id serial primary key, name text)", "SELECT * FROM generate_series(1,10000)"]
  executeMany_ conn ["SELECT * FROM generate_series(1,10000)", "INSERT INTO xx(name) values ('test'), ('abc')"]
  execute_ conn "DROP TABLE xx;"

wellFormedPreparedStatements :: HPgConnection -> IO ()
wellFormedPreparedStatements conn = withRollback conn $ do
  executeMany_ conn ["CREATE TABLE xx(num int)", "CREATE TABLE yy()", "INSERT INTO xx (num) SELECT * FROM generate_series(1,10)"]
  queryWith (rowParser @(Only Int)) conn "SELECT COUNT(*) FROM xx WHERE num IN (8, 9)" `shouldReturn` [Only 2]
  queryWith (rowParser @(Only Int)) conn (mkQuery "SELECT COUNT(*) FROM xx WHERE num BETWEEN $1 AND $2" (1 :: Int, 5 :: Int)) `shouldReturn` [Only 5]
  queryWith (rowParser @(Only Int)) conn "SELECT * FROM xx WHERE num < 0" `shouldReturn` []
  execute_ conn "DROP TABLE xx;"

valuesRoundTrip :: HPgConnection -> IO ()
valuesRoundTrip conn = do
  -- TODO: Property-based test to generate the values
  -- TODO: Include NULLs
  -- TODO: Test +-infinity for types where we can
  -- TODO: Test all types in the regions of values close to `minBound`, 0, and `maxBound`
  -- TODO: Test floats, timestamptz and other very granular but discrete type in the regions of values
  --       close to `minBound`, 0, and `maxBound`, with e.g. microsecond precision/fractional values
  -- TODO: Test +-Infinity and NaN for floats and doubles
  let row = ((-49) :: Int, False :: Bool, 2 :: Int16, 3 :: Int32, fromGregorian 1900 02 28, 42 :: Int64, UTCTime (fromGregorian 1999 12 31) 0, '意' :: Char, '&' :: Char, CalendarDiffTime 3 86403)
  queryWith rowParser conn (mkQuery "SELECT $1, $2, $3, $4, $5, $6, $7, $8, $9, $10" row) `shouldReturn` [row]

byteaValuesRoundTrip :: HPgConnection -> PropertyT IO ()
byteaValuesRoundTrip conn = hedgehog $ do
  someBs :: ByteString <- Gen.forAll $ Gen.bytes (Gen.linear 0 50)
  let lazyBs :: LBS.ByteString = LBS.fromStrict someBs
      row = (someBs, lazyBs)
  res <- liftIO $ queryWith rowParser conn (mkQuery "SELECT $1, $2" row)
  res === [row]

dateAndTimestampTzRoundTrip :: HPgConnection -> PropertyT IO ()
dateAndTimestampTzRoundTrip conn = hedgehog $ do
  -- PG type value limits: https://www.postgresql.org/docs/current/datatype-datetime.html
  yearForDate :: Integer <- Gen.forAll $ Gen.integral (Gen.linear (-4712) 5874896)
  yearForTimetz :: Integer <- Gen.forAll $ Gen.integral (Gen.linear (-4712) 294275)
  month :: Int <- Gen.forAll $ Gen.int $ Gen.linear 1 12
  day :: Int <- Gen.forAll $ Gen.int $ Gen.linear 1 32 -- In case there are some weird calendar dates, go all the way to 32
  timeOfDay :: DiffTime <- Gen.forAll $ fmap (picosecondsToDiffTime . (* 1_000_000)) $ Gen.integral $ Gen.linear 0 86_400_000_000 -- Postgres has microsecond precision, so don't go beyond that
  someIntervalLessThan1Month :: NominalDiffTime <- Gen.forAll $ fmap (realToFrac . picosecondsToDiffTime . (* 1_000_000)) $ Gen.integral $ Gen.linear 0 (27 * 86_400_000_000) -- Postgres has microsecond precision, so don't go beyond that
  someNumberOfMonths :: Integer <- Gen.forAll $ Gen.integral $ Gen.linear (-1000) 1000
  let date = fromGregorian yearForDate month day
      timetz = UTCTime (fromGregorian yearForTimetz month day) timeOfDay
      someInterval = CalendarDiffTime someNumberOfMonths someIntervalLessThan1Month
      row = (date, date, timetz, someInterval)
      rowRes = (date, date, UTCTime (fromGregorian 1900 1 1) 13, UTCTime (fromGregorian 1993 4 15) (11 * 3600 + 49 * 60 + 55 + 0.5), timetz, someInterval)
  -- TODO: If we change the server's timezone, does this still round-trip?
  res <- liftIO $ queryWith rowParser conn (mkQuery "SELECT $1, $2, '1900-01-01T00:00:13Z'::timestamptz, '1993-04-15T11:49:55.5Z'::timestamptz, $3, $4" row)
  res === [rowRes]

numericValuesRoundTrip :: HPgConnection -> PropertyT IO ()
numericValuesRoundTrip conn = hedgehog $ do
  floatVal :: Float <- Gen.forAll $ Gen.float $ Gen.exponentialFloatFrom 0 (-1e38) 1e38
  floatVal2 :: Float <- Gen.forAll $ Gen.float $ Gen.linearFracFrom 0 (-1e38) 1e38
  doubleVal :: Double <- Gen.forAll $ Gen.double $ Gen.exponentialFloatFrom 0 (-1e308) 1e308
  doubleVal2 :: Double <- Gen.forAll $ Gen.double $ Gen.linearFracFrom 0 (-1e308) 1e308
  integerVal :: Integer <- Gen.forAll $ (*) <$> (fromIntegral @Int64 <$> Gen.enumBounded) <*> (fromIntegral @Int64 <$> Gen.enumBounded)
  let row = (1.521 :: Double, 1.521 :: Double, 1.521 :: Scientific, floatVal, floatVal2, doubleVal, doubleVal2, integerVal)
      rowRes = (1.521 :: Scientific, 1.5 :: Scientific, 1.521 :: Scientific, floatVal, floatVal2, doubleVal, doubleVal2, integerVal)
  res <- liftIO $ queryWith rowParser conn (mkQuery "SELECT $1::numeric, $2::numeric(4,1), $3, $4, $5, $6, $7, $8" row)
  res === [rowRes]

numericValuesRoundTripEvenWhenTargetTypesAreLarger :: HPgConnection -> PropertyT IO ()
numericValuesRoundTripEvenWhenTargetTypesAreLarger conn = hedgehog $ do
  -- Cannot decode postgres float4 into Haskell Double yet due to precision issues
  -- floatVal :: Float <- Gen.forAll $ Gen.float $ Gen.exponentialFloatFrom 0 (-1e38) 1e38
  int2Val :: Int16 <- Gen.forAll Gen.enumBounded
  int4Val :: Int32 <- Gen.forAll Gen.enumBounded
  int8Val :: Int64 <- Gen.forAll Gen.enumBounded
  let row = (int2Val, int2Val, int2Val, int2Val, int4Val, int4Val, int4Val, int8Val, int8Val)
      rowRes = (fromIntegral int2Val :: Int32, fromIntegral int2Val :: Int64, fromIntegral int2Val :: Integer, fromIntegral int2Val :: Scientific, fromIntegral int4Val :: Int64, fromIntegral int4Val :: Integer, fromIntegral int4Val :: Scientific, fromIntegral int8Val :: Integer, fromIntegral int8Val :: Scientific)
  res <- liftIO $ queryWith rowParser conn (mkQuery "SELECT $1, $2, $3, $4, $5, $6, $7, $8, $9" row)
  res === [rowRes]

dateDecoding :: HPgConnection -> IO ()
dateDecoding conn = do
  let rowRes = (fromGregorian 1999 12 31, fromGregorian 2010 01 01, fromGregorian 2011 07 04, fromGregorian 1981 03 17)
  queryWith rowParser conn (mkQuery "SELECT '1999-12-31'::date, '2010-01-01'::date, '2011-07-04'::date, '1981-03-17'::date" ()) `shouldReturn` [rowRes]

dateEncoding :: HPgConnection -> IO ()
dateEncoding conn = do
  let row = (fromGregorian 1999 12 31, fromGregorian 2010 01 01, fromGregorian 2011 07 04, fromGregorian 1981 03 17)
  queryWith rowParser conn (mkQuery "SELECT $1, $2, $3, $4" row) `shouldReturn` [row]

timestampDecoding :: HPgConnection -> IO ()
timestampDecoding conn = do
  let rowRes = (UTCTime (fromGregorian 1999 12 31) 0, UTCTime (fromGregorian 2010 01 01) 0, UTCTime (fromGregorian 2011 07 04) (secondsToDiffTime 1), UTCTime (fromGregorian 1981 03 17) 43200, UTCTime (fromGregorian 1900 11 30) 43201)
  queryWith rowParser conn (mkQuery "SELECT '1999-12-31'::timestamptz, '2010-01-01'::timestamptz, '2011-07-04T00:00:01Z'::timestamptz, '1981-03-17T12:00:00Z'::timestamptz, '1900-11-30T12:00:01Z'::timestamptz" ()) `shouldReturn` [rowRes]

timestampEncoding :: HPgConnection -> IO ()
timestampEncoding conn = do
  let row = (UTCTime (fromGregorian 1999 12 31) 0, UTCTime (fromGregorian 2010 01 01) 0, UTCTime (fromGregorian 2011 07 04) (secondsToDiffTime 1), UTCTime (fromGregorian 1981 03 17) 0, UTCTime (fromGregorian 1981 03 17) 43200)
  queryWith rowParser conn (mkQuery "SELECT $1, $2, $3, $4, $5" row) `shouldReturn` [row]

lessUsualTypes :: HPgConnection -> IO ()
lessUsualTypes conn = do
  -- Just making sure this doesn't throw is likely enough
  void $ queryWith (rowParser @(Oid, Int, String, Char)) conn "SELECT oid, typlen, typname, typcategory FROM pg_type"
  void $ queryWith (rowParser @(Oid, Int16, String, Char)) conn "SELECT oid, typlen, typname, typcategory FROM pg_type"

preparedStatementWrongTypes :: HPgConnection -> IO ()
preparedStatementWrongTypes conn =
  queryWith (rowParser @(Int, Text)) conn "SELECT 1, 2" `shouldThrow` irrecoverableErrorWithMsg "Query result column types do not match expected column types"

preparedStatementWrongTypesThrowsEvenWithZeroReturnedRows :: HPgConnection -> IO ()
preparedStatementWrongTypesThrowsEvenWithZeroReturnedRows conn =
  -- TODO: Test prepared statement with a query that fails the parser
  queryWith (rowParser @(Int, Text)) conn "SELECT 1, 2 WHERE FALSE -- No rows still throws" `shouldThrow` irrecoverableErrorWithMsg "Query result column types do not match expected column types"

runPipelineWithMultipleStatements :: HPgConnection -> IO ()
runPipelineWithMultipleStatements conn = do
  (resultsOfFirstStmt, resultsOfSecondStmt, resultsOfLastStmt, createTbl) <- runPipeline conn $ (,,,) <$> pipelineL (rowParser @(Int, Int)) "SELECT 1, 2 WHERE FALSE" <*> pipelineS (rowParser @(Int, Int)) "SELECT 3, 4 FROM generate_series(1,2)" <*> pipelineS (rowParser @(Int64, Bool)) "SELECT x, FALSE FROM generate_series(1,3) subq(x)" <*> pipelineCmd "CREATE table test_table()"
  resultsOfFirstStmt `shouldReturn` []
  (resultsOfSecondStmt >>= S.toList_) `shouldReturn` [(3, 4), (3, 4)]
  (resultsOfLastStmt >>= S.toList_) `shouldReturn` [(1, False), (2, False), (3, False)]
  createTbl `shouldReturn` 0
  execute_ conn "DROP TABLE test_table"

runPipelineErrorSemantics :: HPgConnection -> IO ()
runPipelineErrorSemantics conn = do
  -- It's impossible to fetch query results of queries if even one earlier query in
  -- the same pipeline failed. But we expect a good error message.
  (errCmd, cmd2, cmd3) <- runPipeline conn $ (,,) <$> pipelineCmd "SELECT 1/0" <*> pipelineCmd "SELECT 3, 4" <*> pipelineCmd "SELECT 1"
  errCmd `shouldThrow` pgErrorMustContain [(ErrorCode, "22012")]
  cmd2 `shouldThrow` irrecoverableErrorWithMsg "Another query in the same pipeline threw an error"
  cmd3 `shouldThrow` irrecoverableErrorWithMsg "Another query in the same pipeline threw an error"

runPipelineErrorSemanticsUnsupportedCaseStillBehavesWell :: HPgConnection -> IO ()
runPipelineErrorSemanticsUnsupportedCaseStillBehavesWell conn = do
  -- In case it's not a postgres error in a pipelined statement and rather a user error,
  -- we don't promise anything to users, but we test that HPgsql at least won't deadlock
  -- or do something gnarly, and preferrably we throw an informative exception.
  (goodCmd, parserErrCmd, otherCmd) <- runPipeline conn $ (,,) <$> pipelineCmd "SELECT 3,4" <*> pipelineL (rowParser @(Only Bool)) "SELECT 1" <*> pipelineL (rowParser @(Int, Int)) "SELECT 3, 4"
  goodCmd `shouldReturn` 1
  parserErrCmd `shouldThrow` irrecoverableErrorWithMsg "Query result column types do not match expected column types"
  otherCmd `shouldThrow` irrecoverableErrorWithMsg "HPgsql does not support consuming a query's results before consuming all previous queries' results from the same pipeline"

runPipelineRunsInImplicitTransaction :: HPgConnection -> IO ()
runPipelineRunsInImplicitTransaction conn = do
  cmds <- runPipeline conn (traverse pipelineCmd ["CREATE TABLE test()", "SELECT 3, 4", "SELECT (1/0)=42"])
  sequenceA cmds `shouldThrow` pgErrorMustContain [(ErrorCode, "22012")] -- Division by zero
  execute conn "SELECT COUNT(*) FROM test" `shouldThrow` pgErrorMustContain [(ErrorCode, "42P01"), (ErrorHumanReadableMsg, "relation \"test\" does not exist")]

checkCommandCounts :: HPgConnection -> IO ()
checkCommandCounts conn = withRollback conn $ do
  execute_ conn "CREATE TABLE xx(id serial primary key, val int);"
  execute conn "INSERT INTO xx(val) select * from generate_series(1,100)" `shouldReturn` 100
  execute conn "UPDATE xx SET val=val+1" `shouldReturn` 100
  execute conn "DELETE FROM xx WHERE val > 2" `shouldReturn` 99
  execute conn "DELETE FROM xx" `shouldReturn` 1
  execute conn "DROP TABLE xx;" `shouldReturn` 0

queryTransactionStatusInAllTransactionStates :: HPgConnection -> IO ()
queryTransactionStatusInAllTransactionStates conn = do
  -- First a nice case
  connectionTransactionStatus conn `shouldReturn` TransIdle
  execute_ conn "BEGIN"
  connectionTransactionStatus conn `shouldReturn` TransInTrans
  execute_ conn "COMMIT"
  connectionTransactionStatus conn `shouldReturn` TransIdle

  -- Now a failure
  execute_ conn "BEGIN"
  connectionTransactionStatus conn `shouldReturn` TransInTrans
  execute_ conn "SELECT 1/0" `shouldThrow` pgErrorMustContain []
  connectionTransactionStatus conn `shouldReturn` TransInError
  execute_ conn "ROLLBACK"
  connectionTransactionStatus conn `shouldReturn` TransIdle

sendNotifAndReceiveIt :: HPgConnection -> IO ()
sendNotifAndReceiveIt conn = do
  let currentPid = getBackendPid conn
  execute_ conn "LISTEN test_chan"
  execute_ conn "NOTIFY test_chan"
  execute_ conn "NOTIFY test_chan, 'hello'"
  getNotification conn `shouldReturn` NotificationResponse {notifierPid = currentPid, channelName = "test_chan", notifPayload = ""}
  getNotification conn `shouldReturn` NotificationResponse {notifierPid = currentPid, channelName = "test_chan", notifPayload = "hello"}
  execute_ conn "NOTIFY test_chan, 'hello again'"
  execute_ conn "SELECT 91" -- Ensure NotificationResponse can be received at any time
  getNotification conn `shouldReturn` NotificationResponse {notifierPid = currentPid, channelName = "test_chan", notifPayload = "hello again"}

mixedOrderSendAndReceiveNotifications :: HPgConnection -> IO ()
mixedOrderSendAndReceiveNotifications conn = do
  let currentPid = getBackendPid conn
  execute_ conn "LISTEN \"tést_chãn\""
  execute_ conn "NOTIFY \"tést_chãn\", 'hello'"
  getNotification conn `shouldReturn` NotificationResponse {notifierPid = currentPid, channelName = "tést_chãn", notifPayload = "hello"}
  withAsync (getNotification conn) $ \asyncNotif -> do
    threadDelay 100_000
    -- We have to use a different connection because our
    -- implementation isn't smart enough to keep the pipeline
    -- free for sending queries while `getNotification` is blocked.
    hpgsqlConnInfo <- testConnInfo
    withConnection hpgsqlConnInfo 10 $ \conn2 -> do
      execute_ conn2 "NOTIFY \"tést_chãn\", 'hello again'"
      wait asyncNotif `shouldReturn` NotificationResponse {notifierPid = getBackendPid conn2, channelName = "tést_chãn", notifPayload = "hello again"}
      execute_ conn2 "NOTIFY \"tést_chãn\", 'hello agáin 2'"
      execute_ conn2 "NOTIFY \"tést_chãn\", 'hello again 3'"
      execute_ conn "NOTIFY \"tést_chãn\", 'hello again ç 4'"
      getNotification conn `shouldReturn` NotificationResponse {notifierPid = getBackendPid conn2, channelName = "tést_chãn", notifPayload = "hello agáin 2"}
      getNotification conn `shouldReturn` NotificationResponse {notifierPid = getBackendPid conn2, channelName = "tést_chãn", notifPayload = "hello again 3"}
    getNotification conn `shouldReturn` NotificationResponse {notifierPid = currentPid, channelName = "tést_chãn", notifPayload = "hello again ç 4"}
    getNotificationNonBlocking conn `shouldReturn` Nothing

getNotificationBlocksAndWorksEvenAfterOrphanedQuery :: HPgConnection -> IO ()
getNotificationBlocksAndWorksEvenAfterOrphanedQuery conn = do
  -- This example deadlocks, and I'm not sure it can be avoided.
  -- By running `getNotification` in a separate thread and then
  -- `wait notifAsync`, `getNotification` will block until the
  -- pipeline is consumed by its original thread or until that
  -- thread dies. But that thread will never die and never consume
  -- query results (or fire another query) because it's blocked on
  -- `wait notifAsync`.
  mainTid <- myThreadId
  execute_ conn "LISTEN test_chan"
  timeout 100_000 (execute_ conn "SELECT pg_sleep(10)") `shouldReturn` Nothing
  putStrLn "A"
  withAsync
    ( do
        getNotifThreadId <- myThreadId
        putStrLn $ "getNotification threadId: " ++ show getNotifThreadId
        getNotification conn
    )
    $ \notifAsync -> do
      secondTid <- myThreadId
      print $ "Main and second thread ids: " ++ show (mainTid, secondTid)
      putStrLn "B"
      hpgsqlConnInfo <- testConnInfo
      putStrLn "C"
      withConnection hpgsqlConnInfo 10 $ \conn2 -> do
        putStrLn "D"
        execute_ conn2 "NOTIFY test_chan, 'hey'"
        putStrLn "E"
        wait notifAsync `shouldReturn` NotificationResponse {notifierPid = getBackendPid conn2, channelName = "test_chan", notifPayload = "hey"}

pendingNotifClearedByBeforeReturningToPool :: HPgConnection -> IO ()
pendingNotifClearedByBeforeReturningToPool conn = do
  execute_ conn "LISTEN test_chan"
  execute_ conn "NOTIFY test_chan, 'hello'"
  beforeReturningToPool conn Nothing
  -- 100ms should be plenty of time to receive a notification if there was one
  timeout 100_000 (getNotification conn) `shouldReturn` Nothing

badTransactionStateMeansException :: HPgConnection -> IO ()
badTransactionStateMeansException conn = do
  execute_ conn "BEGIN"
  beforeReturningToPool conn Nothing `shouldThrow` irrecoverableErrorWithMsg "transaction was left in an invalid state"

copyStatementSucceeding :: HPgConnection -> IO ()
copyStatementSucceeding conn = do
  execute_ conn "CREATE TABLE employee (    employee_id SERIAL PRIMARY KEY    , employee_name TEXT NOT NULL);"
  connectionTransactionStatus conn `shouldReturn` TransIdle
  withCopy
    conn
    "COPY employee FROM STDIN WITH (FORMAT CSV);"
    ( do
        putCopyData conn "5,Dracula\n"
        putCopyData conn "6,The Grinch\n"
        connectionTransactionStatus conn `shouldReturn` TransInTrans
    )
    `shouldReturn` 2
  connectionTransactionStatus conn `shouldReturn` TransIdle
  queryWith (rowParser @(Only Int)) conn "SELECT COUNT(*) FROM employee" `shouldReturn` [Only 2]
  execute_ conn "DROP TABLE employee"

copyError :: HPgConnection -> IO ()
copyError conn = do
  withRollback conn $ do
    execute_ conn "CREATE TABLE employee (employee_id SERIAL PRIMARY KEY, employee_name TEXT NOT NULL);"
    withCopy
      conn
      "COPY employee FROM STDIN WITH (FORMAT CSV);"
      (putCopyData conn "5,Dracula,column-that-does-not-exist\n")
      `shouldThrow` pgErrorMustContain [(ErrorSeverity, "ERROR"), (ErrorCode, "22P04"), (ErrorHumanReadableMsg, "extra data after last expected column")]
    connectionTransactionStatus conn `shouldReturn` TransInError
  execute_ conn "SELECT 1"
  connectionTransactionStatus conn `shouldReturn` TransIdle

raiseNoticeThenRunQuery :: HPgConnection -> IO ()
raiseNoticeThenRunQuery conn = do
  execute_ conn "DO $$ BEGIN\nRAISE NOTICE 'test1'; RAISE NOTICE 'test2';\nEND;\n$$;"
  queryWith (rowParser @(Only Bool)) conn "select FALSE from generate_series(1,2) subq(x)"
    `shouldReturn` [Only False, Only False]

changeParameterStatus :: HPgConnection -> IO ()
changeParameterStatus conn = do
  getParameterStatus conn "standard_conforming_strings" `shouldReturn` Just "on"
  execute_ conn "SET standard_conforming_strings to off"
  getParameterStatus conn "standard_conforming_strings" `shouldReturn` Just "off"
  execute_ conn "SET standard_conforming_strings to on"
  getParameterStatus conn "standard_conforming_strings" `shouldReturn` Just "on"

queryCompositeType :: HPgConnection -> IO ()
queryCompositeType conn = withRollback conn $ do
  -- TODO: Test composite types inside composite types
  -- TODO: Test empty composite type
  -- TODO: Test that composite types with few or more columns than expected generate good error messages
  -- TODO: Test that composite types with the right number of columns but wrong attribute types generates good error messages
  -- TODO: Test that one can create a record type with FromPgField and ToPgField instances!
  execute conn "CREATE TYPE int_and_bool AS (numfield INT, boolfield BOOL);"
  execute conn "CREATE TYPE mixed_bin_text AS (numfield INT, textfield TEXT);"
  let intAndBoolParser = singleColRowParser $ compositeTypeParser DisallowNull $ rowParser @(Int, Bool)
      intAndTextParser = singleColRowParser $ compositeTypeParser DisallowNull $ rowParser @(Int, Text)
      intAndBoolParserNullableFields = singleColRowParser $ compositeTypeParser DisallowNull $ rowParser @(Maybe Int, Maybe Bool)
      intAndBoolParserNullableValue = singleColRowParser $ compositeTypeParser AllowNull $ rowParser @(Int, Bool)
  queryWith intAndBoolParser conn (mkQuery "SELECT ROW($1,$2)::int_and_bool" (14 :: Int, True)) `shouldReturn` [(14, True)]
  queryWith ((,,) <$> intAndBoolParserNullableFields <*> intAndBoolParserNullableFields <*> intAndBoolParserNullableFields) conn (mkQuery "SELECT ROW(NULL,$1)::int_and_bool, ROW($2,NULL)::int_and_bool, ROW(NULL, NULL)::int_and_bool" (True, 27 :: Int)) `shouldReturn` [((Nothing, Just True), (Just 27, Nothing), (Nothing, Nothing))]
  queryWith ((,) <$> intAndBoolParser <*> intAndBoolParser) conn (mkQuery "SELECT ROW($1,$2)::int_and_bool, ROW($3,$4)::int_and_bool" ((-42) :: Int, True, 91 :: Int, False)) `shouldReturn` [((-42, True), (91, False))]
  queryWith intAndTextParser conn (mkQuery "SELECT ROW($1,$2)::mixed_bin_text" (14 :: Int, "abc" :: Text)) `shouldReturn` [(14, "abc")]
  queryWith intAndBoolParserNullableValue conn "SELECT NULL::int_and_bool" `shouldReturn` [Nothing]

queryArrayTypes :: HPgConnection -> IO ()
queryArrayTypes conn = withRollback conn $ do
  queryWith (rowParser @(Only (Vector Int))) conn (mkQuery "SELECT ARRAY[$1,$2,$3]" (13 :: Int, 31 :: Int, 45 :: Int)) `shouldReturn` [Only $ Vector.fromList [13 :: Int, 31, 45]]
  queryWith (rowParser @(Only (Vector Int16))) conn (mkQuery "SELECT ARRAY[$1,$2,$3]" (13 :: Int16, 49 :: Int16, 91 :: Int16)) `shouldReturn` [Only $ Vector.fromList [13, 49, 91]]
  queryWith (rowParser @(Only (Vector (Maybe Int16)))) conn (mkQuery "SELECT ARRAY[$1,$2,$3]" (13 :: Int16, Nothing :: Maybe Int16, Just (91 :: Int16))) `shouldReturn` [Only $ Vector.fromList [Just 13, Nothing, Just 91]]
  queryWith (rowParser @(Only (Vector (Maybe Text)))) conn (mkQuery "SELECT ARRAY[$1,$2,$3]" (Just ("Hello" :: Text), Nothing :: Maybe String, Just ("again" :: Text))) `shouldReturn` [Only $ Vector.fromList [Just "Hello", Nothing, Just "again"]]

data MyEnum = Val1 | Val2 | Val3
  deriving stock (Eq, Show)

instance FromPgField MyEnum where
  fieldParser =
    let convert = \case
          "val1" -> Val1
          "val2" -> Val2
          "val3" -> Val3
          _ -> error "Invalid value for MyEnum"
     in convert <$> anyTypeDecoder

instance ToPgField MyEnum where
  toPgField =
    toPgField . \case
      Val1 -> "val1" :: Text
      Val2 -> "val2"
      Val3 -> "val3"

queryEnumTypes :: HPgConnection -> IO ()
queryEnumTypes conn = withRollback conn $ do
  execute conn "CREATE TYPE myenum AS ENUM ('val1', 'val2', 'val3');"
  queryWith (rowParser @(MyEnum, MyEnum, MyEnum, Maybe MyEnum)) conn "SELECT 'val1'::myenum, 'val2'::myenum, 'val3'::myenum, NULL::myenum" `shouldReturn` [(Val1, Val2, Val3, Nothing)]
  queryWith (rowParser @(MyEnum, MyEnum, MyEnum, Maybe MyEnum)) conn (mkQuery "SELECT $1, $2, $3, $4" (Val1, Val2, Val3, Nothing :: Maybe MyEnum)) `shouldReturn` [(Val1, Val2, Val3, Nothing)]

data SomeGenericEnum = EVal1 | EVal2 | EVal3
  deriving stock (Eq, Generic, Show)
  deriving (FromPgField) via (LowerCasedPgEnum SomeGenericEnum)

data SomeGenericRecord = SomeGenericRecord
  { a :: Int,
    b :: SomeGenericEnum,
    c :: Text,
    d :: Bool,
    e :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromPgRow)

data SomeGenericProdType
  = SomeGenericProdType
      Int
      SomeGenericEnum
      Text
      Bool
      Bool
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromPgRow)

queryGenericallyDerivedTypes :: HPgConnection -> IO ()
queryGenericallyDerivedTypes conn = withRollback conn $ do
  execute conn "CREATE TYPE myenum AS ENUM ('eval1', 'eval2', 'eval3');"
  queryWith rowParser conn "SELECT 13, 'eval2'::myenum, 'Some text', true, false" `shouldReturn` [SomeGenericRecord 13 EVal2 "Some text" True False]
  queryWith rowParser conn "SELECT 13, 'eval2'::myenum, 'Some text', true, false" `shouldReturn` [SomeGenericProdType 13 EVal2 "Some text" True False]
  queryWith rowParser conn "SELECT 'eval1'::myenum, 'eval2'::myenum, 'eval3'::myenum" `shouldReturn` [(EVal1, EVal2, EVal3)]

connectingToNonExistingDb :: IO ()
connectingToNonExistingDb = do
  hpgsqlConnInfo <- testConnInfo
  connect hpgsqlConnInfo {database = "non-existing-db"} 10 `shouldThrow` irrecoverableErrorMustContain [(ErrorCode, "3D000"), (ErrorHumanReadableMsg, "database \"non-existing-db\" does not exist")]

connectingWithConnectTimeOptions :: IO ()
connectingWithConnectTimeOptions = do
  hpgsqlConnInfo <- testConnInfo
  withConnection hpgsqlConnInfo {options = "-c my.random_setting=4"} 10 $ \conn -> do
    query conn "SELECT current_setting('my.random_setting')" `shouldReturn` [Only ("4" :: Text)]
    execute_ conn "RESET ALL"
    query conn "SELECT current_setting('my.random_setting')" `shouldReturn` [Only ("4" :: Text)]
    execute_ conn "SET my.random_setting=7"
    query conn "SELECT current_setting('my.random_setting')" `shouldReturn` [Only ("7" :: Text)]
    execute_ conn "RESET ALL"
    query conn "SELECT current_setting('my.random_setting')" `shouldReturn` [Only ("4" :: Text)]

pgErrorMustContain :: [(ErrorDetail, LBS.ByteString)] -> PostgresError -> Bool
pgErrorMustContain expected (PostgresError {pgErrorDetails}) = Map.fromList expected `Map.isSubmapOf` pgErrorDetails

irrecoverableErrorMustContain :: [(ErrorDetail, LBS.ByteString)] -> IrrecoverableHpgsqlError -> Bool
irrecoverableErrorMustContain expected (IrrecoverableHpgsqlError {pgErrorDetails}) = Map.fromList expected `Map.isSubmapOf` pgErrorDetails

irrecoverableErrorWithMsg :: String -> IrrecoverableHpgsqlError -> Bool
irrecoverableErrorWithMsg expectedInfixMsg (IrrecoverableHpgsqlError {hpgsqlDetails}) = expectedInfixMsg `List.isInfixOf` hpgsqlDetails

withRollback :: HPgConnection -> IO a -> IO a
withRollback conn f = do
  execute_ conn "BEGIN"
  res <- try @_ @PostgresError f
  -- On a postgres error, we still need to ROLLBACK, then rethrow
  execute_ conn "ROLLBACK"
  case res of
    Left e -> throw e
    Right v -> pure v

getUniqueNE :: (HasCallStack) => NonEmpty a -> a
getUniqueNE l
  | length l /= 1 = error "NonEmpty list expected to have a single element has more than one"
  | otherwise = NE.head l

-- Note [`timeout` uses the same ThreadId]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- I was under the impression that `timeout` forks a different thread, but
-- it might not, at least under the concurrent RTS, as per official docs.
-- This means interruption safety requires us to cancel and drain active
-- queries even when they were issued from the same ThreadId.
