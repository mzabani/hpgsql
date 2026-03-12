{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module PipelineSpec where

import Data.Int (Int64)
import DbUtils
  ( aroundConn,
    irrecoverableErrorWithMsg,
    pgErrorMustContain,
  )
import HPgsql
import HPgsql.Query (sql)
import qualified Streaming.Prelude as S
import Test.Hspec

spec :: Spec
spec = do
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
    it
      "Multi-statement query is implicit pipeline"
      multiStatementQueryIsImplicitPipeline

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
  errCmd `shouldThrow` pgErrorMustContain "SELECT 1/0" [(ErrorCode, "22012")]
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
  sequenceA cmds `shouldThrow` pgErrorMustContain "SELECT (1/0)=42" [(ErrorCode, "22012")] -- Division by zero
  execute conn "SELECT COUNT(*) FROM test" `shouldThrow` pgErrorMustContain "SELECT COUNT(*) FROM test" [(ErrorCode, "42P01"), (ErrorHumanReadableMsg, "relation \"test\" does not exist")]

multiStatementQueryIsImplicitPipeline :: HPgConnection -> IO ()
multiStatementQueryIsImplicitPipeline conn = do
  query
    conn
    [sql|
      CREATE TEMPORARY TABLE x (i INT) ON COMMIT DROP;
      INSERT INTO x (VALUES (1), (2), (3), (4));
      UPDATE x SET i=i+1;
      SELECT i FROM x ORDER BY i;
   |]
    `shouldReturn` map Only [2, 3, 4, 5 :: Int]
