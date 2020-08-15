module Database.PostgreSQL.Simple
  ( Connection,
    Query,
    query,
    query_,
    execute,
    execute_,
    executeMany,
  )
where

import Data.Int (Int64)
import Database.PostgreSQL.Simple.Internal
import {-# SOURCE #-} Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import qualified HPgsql

query :: (ToRow q, HPgsql.FromPgRow r) => Connection -> Query -> q -> IO [r]
query_ :: (HPgsql.FromPgRow r) => Connection -> Query -> IO [r]
execute :: (ToRow q) => Connection -> Query -> q -> IO Int64
executeMany :: (ToRow q) => Connection -> Query -> [q] -> IO Int64
