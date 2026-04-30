module TransactionSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..), concurrently, runConcurrently)
import Control.Exception.Safe (Exception, SomeException, throwIO, throwString)
import Control.Monad (forM_)
import qualified Data.List as List
import DbUtils (aroundConn)
import Hpgsql
import Hpgsql.Query (sql)
import Hpgsql.Transaction
  ( IsolationLevel (..),
    ReadWriteMode (..),
    beginMode,
    rollback,
    transactionStatus,
    withTransaction,
  )
import Hpgsql.Types (Only)
import Test.Hspec

spec :: Spec
spec = aroundConn $ describe "Hpgsql.Transaction" $ parallel $ do
  it
    "Every combination of IsolationLevel and ReadWriteMode generates a valid BEGIN statement"
    everyBeginCombinationIsValid
  it
    "withTransaction issues ROLLBACK when the inner action throws a synchronous exception"
    withTransactionRollsBackOnSyncException
  it
    "withTransaction issues ROLLBACK when the inner action is killed by an asynchronous exception"
    withTransactionRollsBackOnAsyncException

everyBeginCombinationIsValid :: HPgConnection -> IO ()
everyBeginCombinationIsValid conn =
  forM_ [minBound .. maxBound :: IsolationLevel] $ \il ->
    forM_ [minBound .. maxBound :: ReadWriteMode] $ \rw -> do
      beginMode conn il rw
      transactionStatus conn `shouldReturn` TransInTrans
      rollback conn
      transactionStatus conn `shouldReturn` TransIdle

-- | A synchronous exception thrown from inside 'withTransaction' must cause a
-- @ROLLBACK@ and then propagate unchanged.
withTransactionRollsBackOnSyncException :: HPgConnection -> IO ()
withTransactionRollsBackOnSyncException conn = do
  ( withTransaction conn $ do
      execute_ conn [sql|CREATE TABLE tx_sync_test (v INT NOT NULL)|]
      transactionStatus conn `shouldReturn` TransInTrans
      throwIO SyncBoom
    )
    `shouldThrow` \SyncBoom -> True

  -- After the block: transaction is rolled back and the CREATE TABLE is gone.
  transactionStatus conn `shouldReturn` TransIdle
  query conn [sql|SELECT 1 FROM pg_class WHERE relname = 'tx_sync_test'|]
    `shouldReturn` ([] :: [Only Int])

withTransactionRollsBackOnAsyncException :: HPgConnection -> IO ()
withTransactionRollsBackOnAsyncException conn = do
  ( concurrently
      ( withTransaction conn $ do
          transactionStatus conn `shouldReturn` TransInTrans
          execute_ conn [sql|CREATE TABLE tx_async_test (v INT NOT NULL)|]
          execute_ conn [sql|SELECT pg_sleep(3)|]
      )
      (threadDelay 1000_000 >> throwString "asynchronous boom")
    )
    `shouldThrow` \(ex :: SomeException) -> "asynchronous boom" `List.isInfixOf` show ex

  -- The transaction hasn't been rolled back yet because hpgsql is wise
  -- not to run SQL commands on an asynchronous exception.
  -- It's only right before the next SQL statement that the ROLLBACK will
  -- be sent to postgres.
  transactionStatus conn `shouldReturn` TransActive
  query conn [sql|SELECT 1 FROM pg_class WHERE relname = 'tx_async_test'|]
    `shouldReturn` ([] :: [Only Int])
  transactionStatus conn `shouldReturn` TransIdle

data SyncBoom = SyncBoom
  deriving (Show)

instance Exception SyncBoom
