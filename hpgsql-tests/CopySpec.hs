{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module CopySpec where

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
      "COPY succeeding"
      copyStatementSucceeding
    it
      "COPY error"
      copyError

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
        connectionTransactionStatus conn `shouldReturn` TransActive
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
      `shouldThrow` pgErrorMustContain "COPY employee FROM STDIN" [(ErrorSeverity, "ERROR"), (ErrorCode, "22P04"), (ErrorHumanReadableMsg, "extra data after last expected column")]
    connectionTransactionStatus conn `shouldReturn` TransInError
  execute_ conn "SELECT 1"
  connectionTransactionStatus conn `shouldReturn` TransIdle
