{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module CopySpec where

import Data.Int (Int32)
import Data.Text (Text)
import DbUtils
  ( aroundConn,
    pgErrorMustContain,
    withRollback,
  )
import HPgsql
import HPgsql.Query (sql)
import Test.Hspec

spec :: Spec
spec = do
  aroundConn $ describe "COPY" $ do
    it
      "COPY text format succeeding"
      copyTextFmtStatementSucceeding
    it
      "COPY binary format succeeding"
      copyBinaryFmtStatementSucceeding
    it
      "COPY error"
      copyError

copyTextFmtStatementSucceeding :: HPgConnection -> IO ()
copyTextFmtStatementSucceeding conn = withRollback conn $ do
  execute_ conn "CREATE TABLE employee (    employee_id SERIAL PRIMARY KEY    , employee_name TEXT NOT NULL);"
  transactionStatus conn `shouldReturn` TransInTrans
  withCopy_
    conn
    "COPY employee FROM STDIN WITH (FORMAT CSV);"
    ( do
        putCopyData conn "5,Dracula\n"
        putCopyData conn "6,The Grinch\n"
        transactionStatus conn `shouldReturn` TransActive
    )
    `shouldReturn` 2
  transactionStatus conn `shouldReturn` TransInTrans
  queryWith (rowParser @(Only Int)) conn "SELECT COUNT(*) FROM employee" `shouldReturn` [Only 2]
  execute_ conn "DROP TABLE employee"

copyBinaryFmtStatementSucceeding :: HPgConnection -> IO ()
copyBinaryFmtStatementSucceeding conn = withRollback conn $ do
  execute_ conn "CREATE TABLE employee (    employee_id SERIAL PRIMARY KEY    , employee_name TEXT NOT NULL);"
  transactionStatus conn `shouldReturn` TransInTrans
  copyFromL
    conn
    "COPY employee FROM STDIN WITH (FORMAT BINARY);"
    [(5 :: Int32, "Dracula" :: String), (6, "The Grinch")]
    `shouldReturn` 2
  transactionStatus conn `shouldReturn` TransInTrans
  queryWith (rowParser @(Only Int)) conn "SELECT COUNT(*) FROM employee" `shouldReturn` [Only 2]
  execute_ conn "DROP TABLE employee"

copyError :: HPgConnection -> IO ()
copyError conn = do
  withRollback conn $ do
    execute_ conn "CREATE TABLE employee (employee_id SERIAL PRIMARY KEY, employee_name TEXT NOT NULL);"
    withCopy_
      conn
      "COPY employee FROM STDIN WITH (FORMAT CSV);"
      (putCopyData conn "5,Dracula,column-that-does-not-exist\n")
      `shouldThrow` pgErrorMustContain "COPY employee FROM STDIN" [(ErrorSeverity, "ERROR"), (ErrorCode, "22P04"), (ErrorHumanReadableMsg, "extra data after last expected column")]
    transactionStatus conn `shouldReturn` TransInError
  execute_ conn "SELECT 1"
  transactionStatus conn `shouldReturn` TransIdle
