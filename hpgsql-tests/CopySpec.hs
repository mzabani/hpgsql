{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module CopySpec where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import DbUtils
  ( aroundConn,
    pgErrorMustContain,
    withRollback,
  )
import Hpgsql
import Hpgsql.Copy (copyFromL, putCopyData, withCopy_)
import Hpgsql.Query (sql)
import Hpgsql.Transaction (transactionStatus)
import Hedgehog (PropertyT, (===))
import qualified Hedgehog as Gen
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Gen
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
  aroundConn $ describe "COPY" $ parallel $ do
    it
      "COPY text format succeeding"
      copyTextFmtStatementSucceeding
    it
      "COPY binary format succeeding"
      copyBinaryFmtStatementSucceeding
    it
      "COPY error"
      copyError

genRows :: Gen.Gen [(Int32, Text)]
genRows = do
  numRows <- Gen.int (Gen.linear 0 1000)
  names <- Gen.list (Gen.singleton numRows) $ Gen.text (Gen.linear 1 50) Gen.alphaNum
  pure $ zip [1 ..] names

copyTextFmtStatementSucceeding :: HPgConnection -> PropertyT IO ()
copyTextFmtStatementSucceeding conn = hedgehog $ do
  rows <- Gen.forAll genRows
  result <- liftIO $ withRollback conn $ do
    execute_ conn "CREATE UNLOGGED TABLE copy_test0 (id INT NOT NULL, name TEXT NOT NULL)"
    withCopy_
      conn
      "COPY copy_test0 FROM STDIN WITH (FORMAT CSV);"
      ( forM_ rows $ \(eid, ename) ->
          putCopyData conn $ TE.encodeUtf8 $ Text.pack (show eid) <> "," <> ename <> "\n"
      )
    query conn "SELECT id, name FROM copy_test0 ORDER BY id"
  result === rows

copyBinaryFmtStatementSucceeding :: HPgConnection -> PropertyT IO ()
copyBinaryFmtStatementSucceeding conn = hedgehog $ do
  rows <- Gen.forAll genRows
  result <- liftIO $ withRollback conn $ do
    execute_ conn "CREATE UNLOGGED TABLE copy_test1 (id INT NOT NULL, name TEXT NOT NULL)"
    copyFromL
      conn
      "COPY copy_test1 FROM STDIN WITH (FORMAT BINARY);"
      rows
    query conn "SELECT id, name FROM copy_test1 ORDER BY id"
  result === rows

copyError :: HPgConnection -> IO ()
copyError conn = do
  withRollback conn $ do
    execute_ conn "CREATE UNLOGGED TABLE employee (employee_id SERIAL PRIMARY KEY, employee_name TEXT NOT NULL);"
    withCopy_
      conn
      "COPY employee FROM STDIN WITH (FORMAT CSV);"
      (putCopyData conn "5,Dracula,column-that-does-not-exist\n")
      `shouldThrow` pgErrorMustContain "COPY employee FROM STDIN" [(ErrorSeverity, "ERROR"), (ErrorCode, "22P04"), (ErrorHumanReadableMsg, "extra data after last expected column")]
    transactionStatus conn `shouldReturn` TransInError
  execute_ conn "SELECT 1"
  transactionStatus conn `shouldReturn` TransIdle
