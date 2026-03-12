{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module BasicTestsSpec where

import Control.Monad (forM_, void)
import Data.Text (Text)
import DbUtils
  ( aroundConn,
    irrecoverableErrorWithMsg,
    pgErrorMustContain,
    withRollback,
  )
import HPgsql
import HPgsql.Query (sql)
import Streaming (Of (..))
import qualified Streaming.Prelude as S
import Test.Hspec

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
    `shouldThrow` pgErrorMustContain "select 1, 2, 3" [(ErrorCode, "08P01"), (ErrorHumanReadableMsg, "bind message has 2 result formats but query has 3 columns")]

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
    `shouldThrow` pgErrorMustContain "select 1/(x - 2) > 0 from generate_series(1,2) subq(x)" [(ErrorCode, "22012"), (ErrorHumanReadableMsg, "division by zero")]
  queryWith (rowParser @(Only Bool)) conn "select FALSE from generate_series(1,2) subq(x)"
    `shouldReturn` [Only False, Only False]

queryThatErrorsBeforeReturningAnyRowsFollowedBySuccessfulQuery :: HPgConnection -> IO ()
queryThatErrorsBeforeReturningAnyRowsFollowedBySuccessfulQuery conn = do
  queryWith (rowParser @(Only Bool)) conn "select 1/0"
    `shouldThrow` pgErrorMustContain "select 1/0" [(ErrorCode, "22012"), (ErrorHumanReadableMsg, "division by zero")]
  queryWith (rowParser @(Only Bool)) conn "select FALSE from generate_series(1,2) subq(x)"
    `shouldReturn` [Only False, Only False]

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

preparedStatementWrongTypes :: HPgConnection -> IO ()
preparedStatementWrongTypes conn =
  queryWith (rowParser @(Int, Text)) conn "SELECT 1, 2" `shouldThrow` irrecoverableErrorWithMsg "Query result column types do not match expected column types"

preparedStatementWrongTypesThrowsEvenWithZeroReturnedRows :: HPgConnection -> IO ()
preparedStatementWrongTypesThrowsEvenWithZeroReturnedRows conn =
  -- TODO: Test prepared statement with a query that fails the parser
  queryWith (rowParser @(Int, Text)) conn "SELECT 1, 2 WHERE FALSE -- No rows still throws" `shouldThrow` irrecoverableErrorWithMsg "Query result column types do not match expected column types"

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
  execute_ conn "SELECT 1/0" `shouldThrow` pgErrorMustContain "SELECT 1/0" []
  connectionTransactionStatus conn `shouldReturn` TransInError
  execute_ conn "ROLLBACK"
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
