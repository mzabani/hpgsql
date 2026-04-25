module PreparedStatementsSpec where

import Control.Monad (forM_)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import DbUtils (aroundConn, withRollback)
import Hpgsql (HPgConnection, Only (..), query)
import Hpgsql.Encoding (rowParser)
import Hpgsql.InternalTypes (Query (..), SingleQuery (..))
import Hpgsql.Pipeline (pipelineL, runPipeline)
import Hpgsql.Query (breakQueryIntoStatements, escapeIdentifier, nonPreparedStatement, preparedStatement, sql, sqlPrep, vALUES)
import Test.Hspec

spec :: Spec
spec = do
  describe "isPrepared flag" $ do
    it "sql quasiquoter, vALUES, escapeIdentifier, and preparedness under concatenation" $ do
      let sqlQuery = [sql|SELECT 1|]
          valuesQuery = vALUES [(1 :: Int, True)]
          identQuery = escapeIdentifier "my_table"
      sqlQuery.isPrepared `shouldBe` False
      valuesQuery.isPrepared `shouldBe` False
      identQuery.isPrepared `shouldBe` False
      ([sqlPrep|INSERT INTO x ^{valuesQuery}|]).isPrepared `shouldBe` True
      ([sqlPrep|SELECT * FROM ^{identQuery};|]).isPrepared `shouldBe` True

    it "preparedStatement sets isPrepared to True" $ do
      let q = [sql|SELECT 1|]
      q.isPrepared `shouldBe` False
      (preparedStatement q).isPrepared `shouldBe` True

    it "nonPreparedStatement sets isPrepared to False" $ do
      let q = preparedStatement [sql|SELECT 1|]
      q.isPrepared `shouldBe` True
      (nonPreparedStatement q).isPrepared `shouldBe` False

    it "breakQueryIntoStatements preserves isPrepared = False" $ do
      let q = [sql|SELECT 1; SELECT 2; SELECT 3|]
      q.isPrepared `shouldBe` False
      assertIsPreparedPropagation q

    it "breakQueryIntoStatements preserves isPrepared = True" $ do
      let q = preparedStatement [sql|SELECT 1; SELECT 2; SELECT 3|]
      q.isPrepared `shouldBe` True
      assertIsPreparedPropagation q

  aroundConn $ describe "Running prepared queries" $ do
    -- TODO: Run tests more than once in the same connection
    it "Prepared query without arguments running twice in the same pipeline for the first time" $ \conn -> forM_ [1, 2 :: Int] $ const $ do
      (l1, l2) <- runPipeline conn $ (,) <$> pipelineL rowParser [sqlPrep|SELECT 42|] <*> pipelineL rowParser [sqlPrep|SELECT 42|]
      l1 `shouldReturn` [Only (42 :: Int)]
      l2 `shouldReturn` [Only (42 :: Int)]

    it "Prepared query with arguments returns correct row" $ \conn -> forM_ [1, 2 :: Int] $ const $ do
      let x = 10 :: Int
          y = 32 :: Int
      query conn [sqlPrep|SELECT #{x} + #{y}|] `shouldReturn` [Only (42 :: Int)]
      withRollback conn $ do
        query conn [sql|SELECT #{y} + #{x}|] `shouldReturn` [Only (42 :: Int)]
        query conn [sqlPrep|SELECT #{y} + #{x}|] `shouldReturn` [Only (42 :: Int)]
      query conn [sqlPrep|SELECT #{4 :: Int} + #{y}|] `shouldReturn` [Only (36 :: Int)]
      query conn [sql|SELECT #{4 :: Int} + #{y}|] `shouldReturn` [Only (36 :: Int)]

assertIsPreparedPropagation :: Query -> IO ()
assertIsPreparedPropagation q = do
  let stmts = NE.toList $ breakQueryIntoStatements q
  mapM_ (\s -> isJust s.preparedStmtHash `shouldBe` q.isPrepared) stmts
