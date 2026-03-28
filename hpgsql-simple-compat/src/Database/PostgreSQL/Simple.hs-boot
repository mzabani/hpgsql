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
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Database.PostgreSQL.Simple.Types

query :: (ToRow q, FromRow r) => Connection -> Query -> q -> IO [r]
query_ :: (FromRow r) => Connection -> Query -> IO [r]
execute :: (ToRow q) => Connection -> Query -> q -> IO Int64
executeMany :: (ToRow q) => Connection -> Query -> [q] -> IO Int64
