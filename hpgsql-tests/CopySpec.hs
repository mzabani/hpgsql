module CopySpec where

import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int32, Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import DbUtils
  ( aroundConn,
    pgErrorMustContain,
    withRollback,
  )
import Hedgehog (PropertyT, (===))
import qualified Hedgehog as Gen
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Gen
import Hpgsql
import Hpgsql.Copy (copyFrom, copyStart, putCopyData, putCopyError, withCopy_)
import Hpgsql.Query (sql)
import Hpgsql.Transaction (TransactionStatus (..), transactionStatus)
import System.Timeout (timeout)
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
    it
      "putCopyError"
      copyErrorPutCopyError
    it
      "COPY with a SQL syntax error fails promptly instead of hanging"
      copyWithSyntaxErrorDoesNotHang
    it
      "COPY to a non-existent table fails promptly instead of hanging"
      copyToNonExistentTableDoesNotHang

genRows :: Gen.Gen [(Int32, Text, Int64)]
genRows = do
  numRows <- Gen.int (Gen.linear 0 1000)
  names <- Gen.list (Gen.singleton numRows) $ Gen.text (Gen.linear 1 50) Gen.alphaNum
  numbers <- Gen.list (Gen.singleton numRows) $ Gen.int64 (Gen.linear (-100) 100)
  pure $ zip3 [1 ..] names numbers

copyTextFmtStatementSucceeding :: HPgConnection -> PropertyT IO ()
copyTextFmtStatementSucceeding conn = hedgehog $ do
  rows <- Gen.forAll genRows
  result <- liftIO $ withRollback conn $ do
    execute_ conn "CREATE UNLOGGED TABLE copy_test0 (id INT NOT NULL, name TEXT NOT NULL, some_num BIGINT)"
    withCopy_
      conn
      "COPY copy_test0 FROM STDIN WITH (FORMAT CSV);"
      ( forM_ rows $ \(eid, ename, somenum) ->
          putCopyData conn $ TE.encodeUtf8 $ Text.pack (show eid) <> "," <> ename <> "," <> Text.pack (show somenum) <> "\n"
      )
    query conn "SELECT id, name, some_num FROM copy_test0 ORDER BY id"
  result === rows

copyBinaryFmtStatementSucceeding :: HPgConnection -> PropertyT IO ()
copyBinaryFmtStatementSucceeding conn = hedgehog $ do
  rows <- Gen.forAll genRows
  result <- liftIO $ withRollback conn $ do
    execute_ conn "CREATE UNLOGGED TABLE copy_test1 (id INT NOT NULL, name TEXT NOT NULL, some_num BIGINT)"
    copyFrom
      conn
      "COPY copy_test1 FROM STDIN WITH (FORMAT BINARY);"
      rows
    query conn "SELECT id, name, some_num FROM copy_test1 ORDER BY id"
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

-- | Invalid syntax in COPY statements makes postgres hand us an error message
-- before Parse/Bind, so that's worth testing. In the past hpgsql would hang in such
-- a case, waiting for ReadyForQuery forever.
copyWithSyntaxErrorDoesNotHang :: HPgConnection -> IO ()
copyWithSyntaxErrorDoesNotHang conn = do
  result <-
    timeout 5_000_000 $
      withCopy_ conn "COPY FROM STDIN WITH (FORMAT CSV);" (pure ())
        `shouldThrow` pgErrorMustContain "COPY FROM STDIN" [(ErrorSeverity, "ERROR"), (ErrorCode, "42601")]
  result `shouldSatisfy` isJust
  void $ execute conn "SELECT 1" -- Connection should be healthy for another query

-- | Same concern as 'copyWithSyntaxErrorDoesNotHang', but for a statement that fails at
-- Execute time (a missing table) rather than at Parse time.
copyToNonExistentTableDoesNotHang :: HPgConnection -> IO ()
copyToNonExistentTableDoesNotHang conn = do
  result <-
    timeout 5_000_000 $
      withCopy_ conn "COPY table_that_does_not_exist_xyz FROM STDIN WITH (FORMAT CSV);" (pure ())
        `shouldThrow` pgErrorMustContain "COPY table_that_does_not_exist_xyz FROM STDIN" [(ErrorSeverity, "ERROR")]
  result `shouldSatisfy` isJust
  void $ execute conn "SELECT 1" -- Connection should be healthy for another query

copyErrorPutCopyError :: HPgConnection -> IO ()
copyErrorPutCopyError conn = do
  withRollback conn $ do
    execute_ conn "CREATE UNLOGGED TABLE employee (employee_id SERIAL PRIMARY KEY, employee_name TEXT NOT NULL);"
    copyStart
      conn
      "COPY employee FROM STDIN WITH (FORMAT CSV);"
    putCopyData conn "5,Dracula\n"
    putCopyError conn "Abort"
    transactionStatus conn `shouldReturn` TransInError
  execute_ conn "SELECT 1"
  transactionStatus conn `shouldReturn` TransIdle
