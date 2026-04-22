module Hpgsql.Transaction (IsolationLevel (..), ReadWriteMode (..), begin, commit, rollback, withTransaction, withTransactionMode, beginMode, transactionStatus) where

import qualified Control.Concurrent.STM as STM
import Control.Exception.Safe (Exception (..), bracketWithError, throw, tryAny)
import Control.Monad (unless)
import Hpgsql.Internal (execute_, fullTransactionStatus, transactionStatus)
import Hpgsql.InternalTypes (HPgConnection (..), InternalConnectionState (..), IrrecoverableHpgsqlError)
import Hpgsql.Query (Query)
import Hpgsql.TypeInfo (TransactionStatus (..))

-- The types and constructors here have matching names to postgresql-simple where
-- I thought sameness would be convenient. The implementation is of course our
-- own, and I did choose to keep some things different out of my preferences.

data IsolationLevel
  = DefaultIsolationLevel
  | ReadUncommitted
  | ReadCommitted
  | RepeatableRead
  | Serializable
  deriving (Show, Eq, Ord, Enum, Bounded)

data ReadWriteMode
  = DefaultReadWriteMode
  | ReadWrite
  | ReadOnly
  deriving (Show, Eq, Ord, Enum, Bounded)

begin :: HPgConnection -> IO ()
begin conn = beginMode conn DefaultIsolationLevel DefaultReadWriteMode

beginMode :: HPgConnection -> IsolationLevel -> ReadWriteMode -> IO ()
beginMode conn il rw = do
  let readWrite = case rw of
        ReadWrite -> Just "READ WRITE"
        ReadOnly -> Just "READ ONLY"
        DefaultReadWriteMode -> Nothing
      isolLvl = case il of
        DefaultIsolationLevel -> Nothing
        Serializable -> Just "ISOLATION LEVEL SERIALIZABLE"
        RepeatableRead -> Just "ISOLATION LEVEL REPEATABLE READ"
        ReadCommitted -> Just "ISOLATION LEVEL READ COMMITTED"
        ReadUncommitted -> Just "ISOLATION LEVEL READ UNCOMMITTED"
  execute_ conn $ "BEGIN " <> withComma readWrite isolLvl
  where
    withComma :: Maybe Query -> Maybe Query -> Query
    withComma mv1 mv2 = case (mv1, mv2) of
      (Just v1, Just v2) -> v1 <> "," <> v2
      (Just v1, Nothing) -> v1
      (Nothing, Just v2) -> v2
      (Nothing, Nothing) -> ""

commit :: HPgConnection -> IO ()
commit conn = execute_ conn "COMMIT"

rollback :: HPgConnection -> IO ()
rollback conn = execute_ conn "ROLLBACK"

-- | Runs the supplied function inside a transaction with the database's
-- default isolation level and read-write mode, i.e. `BEGIN`s a transaction,
-- runs the supplied function and then `COMMIT`s if there are no exceptions.
-- In case the supplied function throws an exception, this runs `ROLLBACK`.
-- In case an asynchronous exception is thrown, Hpgsql ensures a `ROLLBACK`
-- will be issued before your next query on the same connection.
withTransaction :: HPgConnection -> IO a -> IO a
withTransaction conn = withTransactionMode conn DefaultIsolationLevel DefaultReadWriteMode

-- | Runs the supplied function inside a transaction with the supplied
-- isolation level and read-write mode, i.e. `BEGIN`s a transaction,
-- runs the supplied function and then `COMMIT`s if there are no exceptions.
-- In case the supplied function throws an exception, this runs `ROLLBACK`.
-- In case an asynchronous exception is thrown, Hpgsql ensures a `ROLLBACK`
-- will be issued before your next query on this connection.
withTransactionMode :: HPgConnection -> IsolationLevel -> ReadWriteMode -> IO a -> IO a
withTransactionMode conn il rw f = bracketWithError (beginMode conn il rw) cleanup $ \() -> do
  res <- tryAny f
  case res of
    Left ex -> case fromException ex of
      Just (_ :: IrrecoverableHpgsqlError) -> throw ex -- Rethrow internal errors
      Nothing -> do
        -- In case of a synchronous exception, we rollback synchronously.
        -- If this is interrupted:
        -- - Before ROLLBACK is sent, `cleanup` will enqueue a "ROLLBACK".
        -- - After ROLLBACK is sent but before it finishes, a ROLLBACK will still be enqueued
        --   (check `cleanup`), which means:
        --      - If ROLLBACK has already completed by the time the new ROLLBACK is meant to be sent,
        --        the new ROLLBACK will produce a "WARNING: there is no transaction in progress". This
        --        isn't great, but is mostly harmless.
        --      - If ROLLBACK is cancelled by the future ROLLBACK, all is good as well.
        -- - After ROLLBACK is sent and completed, `cleanup` won't enqueue a new ROLLBACK, and all is well.
        --
        -- We rollback here, not in `cleanup`, because that runs with async exceptions masked
        rollback conn >> throw ex -- Reminder that this can be interrupted before/after "rollback" is sent
    Right v -> do
      -- If this is interrupted:
      -- - Before COMMIT is sent, it's equivalent to failing in the middle of the user-supplied action
      -- - After COMMIT is sent but before it's finished, a ROLLBACK will still be enqueued
      --   (check `cleanup`), which means:
      --      - If COMMIT has already completed by the time ROLLBACK is meant to be sent, ROLLBACK will
      --        produce a "WARNING: there is no transaction in progress". This isn't great, but
      --        is mostly harmless.
      --      - If COMMIT is cancelled by the future ROLLBACK, all is good as well.
      -- - After COMMIT is sent and completed, this is just interruption right after a successful
      --   operation, and `cleanup` won't enqueue a ROLLBACK.
      commit conn
      pure v
  where
    cleanup mEx () = case mEx of
      Nothing -> pure ()
      Just (fromException -> (Just (_ :: IrrecoverableHpgsqlError))) -> pure () -- Do nothing if an internal error was thrown, just let it propagate
      Just _ex -> do
        -- print _ex
        -- Mark ROLLBACK to be sent before next command
        STM.atomically $ do
          (_, txnStatus) <- fullTransactionStatus conn.internalConnectionState
          -- The rollback in case of a synchronous exception may have run, so we
          -- don't need another one in that case.
          unless (txnStatus == TransIdle) $ do
            st <- STM.readTVar conn.internalConnectionState
            STM.writeTVar conn.internalConnectionState st {mustIssueRollbackBeforeNextCommand = True}
