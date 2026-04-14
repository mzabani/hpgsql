{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

------------------------------------------------------------------------------

------------------------------------------------------------------------------

-- |
-- Module:      Database.PostgreSQL.Simple.Cursor
-- Copyright:   (c) 2011-2012 Leon P Smith
--              (c) 2017 Bardur Arantsson
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
module Database.PostgreSQL.Simple.Cursor
  ( -- * Types
    Cursor,

    -- * Cursor management
    declareCursor,
    closeCursor,

    -- * Folding over rows from a cursor
    foldForward,
    foldForwardWithParser,
  )
where

import Control.Exception as E
import Control.Monad (foldM, unless, void)
import Data.String (fromString)
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.Internal as Base hiding (result, row)
import Database.PostgreSQL.Simple.Transaction
import Database.PostgreSQL.Simple.Types (Query (..))
import HPgsql (Query, execute_, queryWithM)
import HPgsql.Encoding (RowParserMonadic)
import HPgsql.Query (escapeIdentifier, sql)

-- | Cursor within a transaction.
data Cursor = Cursor !Database.PostgreSQL.Simple.Types.Query !Connection

-- | Declare a temporary cursor. The cursor is given a
-- unique name for the given connection.
declareCursor :: Connection -> HPgsql.Query -> IO Cursor
declareCursor conn q = mapHpgsqlErrors $ do
  name <- newTempName conn
  void $ HPgsql.execute_ (hpgConn conn) [sql|DECLARE ^{escapeIdentifier (fromQuery name)} NO SCROLL CURSOR FOR ^{q}|]
  return $ Cursor name conn

-- | Close the given cursor.
closeCursor :: Cursor -> IO ()
closeCursor (Cursor name conn) =
  (void $ mapHpgsqlErrors $ HPgsql.execute_ (hpgConn conn) [sql|CLOSE ^{escapeIdentifier $ fromQuery name}|]) `E.catch` \ex ->
    -- Don't throw exception if CLOSE failed because the transaction is
    -- aborted.  Otherwise, it will throw away the original error.
    unless (isFailedTransactionError ex) $ throwIO ex

-- | Fold over a chunk of rows from the given cursor, calling the
-- supplied fold-like function on each row as it is received. In case
-- the cursor is exhausted, a 'Left' value is returned, otherwise a
-- 'Right' value is returned.
foldForwardWithParser :: Cursor -> RowParserMonadic r -> Int -> (a -> r -> IO a) -> a -> IO (Either a a)
foldForwardWithParser (Cursor name conn) parser chunkSize f a0 = mapHpgsqlErrors $ do
  let q =
        [sql|FETCH FORWARD ^{fromString (show chunkSize)} FROM ^{escapeIdentifier (fromQuery name)}|]
  rows <- queryWithM parser (hpgConn conn) q
  case rows of
    [] -> pure $ Left a0
    _ -> Right <$> foldM f a0 rows

-- | Fold over a chunk of rows, calling the supplied fold-like function
-- on each row as it is received. In case the cursor is exhausted,
-- a 'Left' value is returned, otherwise a 'Right' value is returned.
foldForward :: (FromRow r) => Cursor -> Int -> (a -> r -> IO a) -> a -> IO (Either a a)
foldForward cursor = foldForwardWithParser cursor fromRow
