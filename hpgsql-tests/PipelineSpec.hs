{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module PipelineSpec where

import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import DbUtils
  ( aroundConn,
    irrecoverableErrorWithMsg,
    pgErrorMustContain,
    withRollback,
  )
import HPgsql
import HPgsql.Query (sql)
import Hedgehog
import qualified Hedgehog as Gen
import qualified Hedgehog.Gen as Gen
import qualified Streaming.Prelude as S
import qualified Streaming.Prelude as Streaming
import Test.Hspec
import Test.Hspec.Hedgehog

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
    it "Run varied pipelines" runVariedPipelines
    it "Run varied pipelines with error statement" runVariedPipelinesWithError

data SomeRowReturningStatementTest = forall a. SomeRowReturningStatementTest (RowReturningStatementTest a)

instance Show SomeRowReturningStatementTest where
  show (SomeRowReturningStatementTest s) = show s

-- | Row parser, query, function to check query results and expected results length.
data RowReturningStatementTest a = RowsStatement ((RowParser a), Query, [a] -> IO (), Int64)

-- | Query and expected returned count.
data CountReturningStatementTest = CountReturningStatement (Query, Int64)
  deriving stock (Show)

-- | Query and expected exception.
data FailingStatementTest = FailingStatement (Query, PostgresError -> Bool)

instance Show FailingStatementTest where
  show (FailingStatement (q, _)) = show q

instance Show (RowReturningStatementTest a) where
  show _ = "some-statement"

exampleStmt :: [SomeRowReturningStatementTest] =
  [ SomeRowReturningStatementTest $ RowsStatement (rowParser @(Int, Int), "SELECT 3, 4 FROM generate_series(1,2)", (`shouldBe` [(3, 4), (3, 4)]), 2),
    SomeRowReturningStatementTest $ RowsStatement (rowParser @(Int, Int), "SELECT 1, 2 WHERE FALSE", (`shouldBe` []), 0),
    SomeRowReturningStatementTest $ RowsStatement (rowParser @(Bool, Bool), [sql|SELECT #{True}, #{False}|], (`shouldBe` [(True, False)]), 1),
    -- Queries with multiple statements: we expect only the results of the last statement
    SomeRowReturningStatementTest $ RowsStatement (rowParser @(Only Int64), [sql|SELECT 1; SELECT 34; SELECT * FROM generate_series(11,17)|], (`shouldBe` map Only [11 .. 17]), 7)
  ]

countReturningStmts :: [CountReturningStatementTest] =
  [ CountReturningStatement ("CREATE TABLE a(); DROP table a;", 0),
    CountReturningStatement ("UPDATE some_data SET i=i", 100),
    CountReturningStatement ("UPDATE some_data SET i=i WHERE i<=50", 50),
    -- Queries with multiple statements: we expect only the results of the last statement
    CountReturningStatement ([sql|UPDATE some_data SET i=i WHERE i<=#{2::Int}; UPDATE some_data SET i=i WHERE i<=#{17::Int64}|], 17),
    CountReturningStatement ([sql|INSERT INTO some_data SELECT i FROM some_data WHERE #{False}|], 0)
  ]

-- TODO: Have failing statements that return rows and that return counts, and
-- have failing statements that do yield some rows before failing, and have
-- even statements with bad row parsers?
errorStmts :: [FailingStatementTest]
errorStmts =
  [ FailingStatement ("SELECT 1/0", pgErrorMustContain "SELECT 1/0" [(ErrorCode, "22012")]),
    FailingStatement ("SELECT 1/(100 - x) FROM generate_series(1,200) subq(x)", pgErrorMustContain "SELECT 1/(100 - x) FROM generate_series(1,200) subq(x)" [(ErrorCode, "22012")]),
    FailingStatement ("INSERT INTO some_data (i) SELECT * FROM generate_series(1,100)", pgErrorMustContain "INSERT INTO some_data (i) SELECT * FROM generate_series(1,100)" [(ErrorCode, "23505")]),
    -- With multiple statements, the error message should contain only the one that truly fails
    FailingStatement ("SELECT 1; SELECT 3; /* just this query */ SELECT 1/0", \pgErr -> pgErrorMustContain "" [(ErrorCode, "22012")] pgErr && failedStatement pgErr == " /* just this query */ SELECT 1/0"),
    FailingStatement ("SELECT 1;SELECT /* just this query */ 1/0; SELECT 17", \pgErr -> pgErrorMustContain "" [(ErrorCode, "22012")] pgErr && failedStatement pgErr == "SELECT /* just this query */ 1/0;")
  ]

runVariedPipelines :: HPgConnection -> PropertyT IO ()
runVariedPipelines conn = hedgehog $ do
  SomeRowReturningStatementTest (RowsStatement (rp1, qry1, check1, expectedLen1)) <- Gen.forAll $ Gen.element exampleStmt
  SomeRowReturningStatementTest (RowsStatement (rp2, qry2, check2, expectedLen2)) <- Gen.forAll $ Gen.element exampleStmt
  SomeRowReturningStatementTest (RowsStatement (rp3, qry3, check3, expectedLen3)) <- Gen.forAll $ Gen.element exampleStmt
  CountReturningStatement (countQry1, countExpectedLen1) <- Gen.forAll $ Gen.element countReturningStmts
  CountReturningStatement (countQry2, countExpectedLen2) <- Gen.forAll $ Gen.element countReturningStmts
  CountReturningStatement (countQry3, countExpectedLen3) <- Gen.forAll $ Gen.element countReturningStmts
  liftIO $ withRollback conn $ do
    execute_ conn "CREATE TEMPORARY TABLE some_data ON COMMIT DROP AS SELECT generate_series AS i FROM generate_series(1,100)"
    (listRes1, listRes2, listRes3, streamRes1, streamRes2, streamRes3, countRes1, countRes2, countRes3, countStmtCountRes1, countStmtCountRes2, countStmtCountRes3) <-
      runPipeline conn $
        (,,,,,,,,,,,)
          <$> pipelineL rp1 qry1
          <*> pipelineL rp2 qry2
          <*> pipelineL rp3 qry3
          <*> pipelineS rp1 qry1
          <*> pipelineS rp2 qry2
          <*> pipelineS rp3 qry3
          <*> pipelineCmd qry1
          <*> pipelineCmd qry2
          <*> pipelineCmd qry3
          <*> pipelineCmd countQry1
          <*> pipelineCmd countQry2
          <*> pipelineCmd countQry3
    listRes1 >>= check1
    listRes2 >>= check2
    listRes3 >>= check3
    streamRes1 >>= Streaming.toList_ >>= check1
    streamRes2 >>= Streaming.toList_ >>= check2
    streamRes3 >>= Streaming.toList_ >>= check3
    countRes1 >>= (`shouldBe` expectedLen1)
    countRes2 >>= (`shouldBe` expectedLen2)
    countRes3 >>= (`shouldBe` expectedLen3)
    countStmtCountRes1 >>= (`shouldBe` countExpectedLen1)
    countStmtCountRes2 >>= (`shouldBe` countExpectedLen2)
    countStmtCountRes3 >>= (`shouldBe` countExpectedLen3)

runVariedPipelinesWithError :: HPgConnection -> PropertyT IO ()
runVariedPipelinesWithError conn = hedgehog $ do
  SomeRowReturningStatementTest (RowsStatement (rp1, qry1, check1, expectedLen1)) <- Gen.forAll $ Gen.element exampleStmt
  CountReturningStatement (countQry1, countExpectedLen1) <- Gen.forAll $ Gen.element countReturningStmts
  FailingStatement (errStmt, checkExpectedErr) <- Gen.forAll $ Gen.element errorStmts
  liftIO $ withRollback conn $ do
    execute_ conn "CREATE TEMPORARY TABLE some_data (i INT PRIMARY KEY) ON COMMIT DROP; INSERT INTO some_data (i) SELECT * FROM generate_series(1,100)"
    (listRes1, streamRes1, countRes1, countStmtCountRes1, errStmtRes, willFailListRes1, willFailStreamRes1, willFailCountRes1, willFailStmtCountRes1) <-
      runPipeline conn $
        (,,,,,,,,)
          <$> pipelineL rp1 qry1
          <*> pipelineS rp1 qry1
          <*> pipelineCmd qry1
          <*> pipelineCmd countQry1
          <*> pipelineCmd errStmt
          <*> pipelineL rp1 qry1
          <*> pipelineS rp1 qry1
          <*> pipelineCmd qry1
          <*> pipelineCmd countQry1
    listRes1 >>= check1
    streamRes1 >>= Streaming.toList_ >>= check1
    countRes1 >>= (`shouldBe` expectedLen1)
    countStmtCountRes1 >>= (`shouldBe` countExpectedLen1)
    -- errStmtRes
    errStmtRes `shouldThrow` checkExpectedErr
    willFailListRes1 `shouldThrow` irrecoverableErrorWithMsg "Another query in the same pipeline threw an error"
    (willFailStreamRes1 >>= Streaming.effects) `shouldThrow` irrecoverableErrorWithMsg "Another query in the same pipeline threw an error"
    willFailCountRes1 `shouldThrow` irrecoverableErrorWithMsg "Another query in the same pipeline threw an error"
    willFailStmtCountRes1 `shouldThrow` irrecoverableErrorWithMsg "Another query in the same pipeline threw an error"

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
  -- parserErrCmd `shouldThrow` irrecoverableErrorWithMsg "Query result column types do not match expected column types"
  parserErrCmd `shouldThrow` irrecoverableErrorWithMsg "Query"

-- The next exception doesn't throw the most informative exception,
-- but for now we don't care.
-- otherCmd `shouldThrow` irrecoverableErrorWithMsg "Query result column types do not match expected column types"

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
