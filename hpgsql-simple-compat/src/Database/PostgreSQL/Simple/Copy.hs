{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

------------------------------------------------------------------------------

------------------------------------------------------------------------------

-- |
-- Module:      Database.PostgreSQL.Simple.Copy
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- mid-level support for COPY IN and COPY OUT.   See
-- <https://www.postgresql.org/docs/9.5/static/sql-copy.html> for
-- more information.
--
-- To use this binding,  first call 'copy' with an appropriate
-- query as documented in the link above.  Then, in the case of a
-- @COPY TO STDOUT@ query,  call 'getCopyData' repeatedly until it
-- returns 'CopyOutDone'.   In the case of a @COPY FROM STDIN@
-- query,  call 'putCopyData' repeatedly and then finish by calling
-- either 'putCopyEnd' to proceed or 'putCopyError' to abort.
--
-- You cannot issue another query on the same connection while a copy
-- is ongoing; this will result in an exception.   It is harmless to
-- concurrently call @getNotification@ on a connection while it is in
-- a @CopyIn@ or @CopyOut@ state,  however be aware that current versions
-- of the PostgreSQL backend will not deliver notifications to a client
-- while a transaction is ongoing.
module Database.PostgreSQL.Simple.Copy
  ( copy,
    copy_,
    CopyOutResult (..),
    -- foldCopyData,
    -- getCopyData,
    putCopyData,
    putCopyEnd,
    -- putCopyError,
  )
where

import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.HpgsqlUtils (toHpgsqlQuery)
import Database.PostgreSQL.Simple.Internal hiding (result, row)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Database.PostgreSQL.Simple.Types
import qualified Hpgsql
import qualified Hpgsql.Copy

-- | Issue a @COPY FROM STDIN@ or @COPY TO STDOUT@ query.   In the former
--   case, the connection's state will change to @CopyIn@;  in the latter,
--   @CopyOut@.  The connection must be in the ready state in order
--   to call this function.  Performs parameter substitution.
copy :: (ToRow params) => Connection -> Query -> params -> IO ()
copy conn template qs = do
  let qry = toHpgsqlQuery template qs
  doCopy "Database.PostgreSQL.Simple.Copy.copy" conn qry

-- | Issue a @COPY FROM STDIN@ or @COPY TO STDOUT@ query.   In the former
--   case, the connection's state will change to @CopyIn@;  in the latter,
--   @CopyOut@.  The connection must be in the ready state in order
--   to call this function.  Does not perform parameter substitution.
copy_ :: Connection -> Query -> IO ()
copy_ conn template = do
  let qry = toHpgsqlQuery template ()
  doCopy "Database.PostgreSQL.Simple.Copy.copy_" conn qry

doCopy :: B.ByteString -> Connection -> Hpgsql.Query -> IO ()
doCopy _funcName conn q = mapHpgsqlErrors $ do
  Hpgsql.Copy.copyStart (hpgConn conn) q

data CopyOutResult
  = -- | Data representing either exactly
    --   one row of the result,  or header
    --   or footer data depending on format.
    CopyOutRow !B.ByteString
  | -- | No more rows, and a count of the
    --   number of rows returned.
    CopyOutDone {-# UNPACK #-} !Int64
  deriving (Eq, Typeable, Show)

-- | Feed some data to a @COPY FROM STDIN@ query.  Note that
--   the data does not need to represent a single row,  or even an
--   integral number of rows.  The net result of
--   @putCopyData conn a >> putCopyData conn b@
--   is the same as @putCopyData conn c@ whenever @c == BS.append a b@.
--
--   A connection must be in the @CopyIn@ state in order to call this
--   function,  otherwise a 'SqlError' exception will result.  The
--   connection remains in the @CopyIn@ state after this function
--   is called.
putCopyData :: Connection -> B.ByteString -> IO ()
putCopyData conn dat = mapHpgsqlErrors $ Hpgsql.Copy.putCopyData (hpgConn conn) dat

-- | Completes a @COPY FROM STDIN@ query.  Returns the number of rows
--   processed.
--
--   A connection must be in the @CopyIn@ state in order to call this
--   function,  otherwise a 'SqlError' exception will result.  The
--   connection's state changes back to ready after this function
--   is called.
putCopyEnd :: Connection -> IO Int64
putCopyEnd conn = mapHpgsqlErrors $ Hpgsql.Copy.copyEnd (hpgConn conn)
