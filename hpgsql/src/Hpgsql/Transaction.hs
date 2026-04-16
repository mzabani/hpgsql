module Hpgsql.Transaction (IsolationLevel (..), ReadWriteMode (..), begin, commit, rollback, withTransaction, withTransactionMode, beginMode) where

import Control.Exception.Safe (Exception (..), bracketWithError)
import Data.Maybe (mapMaybe)
import Hpgsql (execute_)
import Hpgsql.InternalTypes (HPgConnection, IrrecoverableHpgsqlError)

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
-- will be issued before your next query on the same connection.
withTransactionMode :: HPgConnection -> IsolationLevel -> ReadWriteMode -> IO a -> IO a
withTransactionMode conn il rw f = bracketWithError (beginMode conn il rw) cleanup (\() -> f)
  where
    cleanup mEx () = case mEx of
      Nothing -> pure ()
      Just (fromException -> (Just (_ :: IrrecoverableHpgsqlError))) -> pure () -- Do nothing if an internal error was thrown, just let it propagate
      Just _ -> rollback conn
