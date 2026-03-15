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
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Database.PostgreSQL.Simple.Types
import HPgsql.Field (FromPgRow, ToPgRow)

query :: (ToPgRow q, FromPgRow r) => Connection -> Query -> q -> IO [r]
query_ :: (FromPgRow r) => Connection -> Query -> IO [r]
execute :: (ToPgRow q) => Connection -> Query -> q -> IO Int64
executeMany :: (ToRow q) => Connection -> Query -> [q] -> IO Int64
