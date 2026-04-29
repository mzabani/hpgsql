-- |
-- Module      : Hpgsql
-- Description : A pure Haskell PostgreSQL driver
--
-- Hpgsql is a PostgreSQL driver written entirely in Haskell, with no bindings to @libpq@.
-- It communicates with the database using the PostgreSQL wire protocol directly.
--
-- = Quick start
--
-- Connect to the database and run a simple query:
--
-- > {-# LANGUAGE QuasiQuotes #-}
-- > import Hpgsql
-- > import Hpgsql.Query (sql)
-- >
-- > main :: IO ()
-- > main = do
-- >   let connstr = ConnString
-- >         { hostname = "localhost"
-- >         , port     = 5432
-- >         , user     = "postgres"
-- >         , password = ""
-- >         , database = "mydb"
-- >         , options  = ""
-- >         }
-- >   -- Connect with a 10-second timeout
-- >   withConnection connstr 10 $ \conn -> do
-- >     rows <- query conn [sql|SELECT 1 + 1|]
-- >     print (rows :: [Only Int])
-- >     -- [Only {fromOnly = 2}]
--
-- = Building queries
--
-- Use the @[sql|...|]@ quasiquoter from "Hpgsql.Query" to build queries. Interpolate Haskell
-- values with @#{}@:
--
-- > let name = "Alice" :: Text
-- > rows <- query conn [sql|SELECT id, email FROM users WHERE name = #{name}|]
-- > print (rows :: [(Int, Text)])
--
-- Values interpolated with @#{}@ are sent as query parameters, so they are safe from SQL injection.
--
-- To embed a 'Query' fragment (e.g. a dynamic table name or a sub-query), use @^{}@:
--
-- > let tableName = escapeIdentifier "users"
-- > rows <- query conn [sql|SELECT * FROM ^{tableName}|]
--
-- You can also use 'Hpgsql.Query.mkQuery' with dollar-numbered query arguments:
--
-- > query conn (mkQuery "SELECT * FROM users WHERE age BETWEEN $1 AND $2" (18 :: Int, 60 :: Int))
--
-- = Fetching results
--
-- * 'query' returns all rows as a list.
-- * 'queryS' returns all rows as a Stream.
-- * 'query1' returns exactly one row, throwing if zero or more than one row is returned.
-- * 'queryMay' returns zero or one row, throwing if more than one is returned.
-- * 'execute' runs a statement and returns the count of affected rows.
-- * 'execute_' runs a statement and discards the result.
--
-- All these methods have equivalents in "Hpgsql.Pipeline" that can be composed together
-- to reduce the number of round-trips.
--
-- = Encoders and decoders
--
-- To define your own encoder and decoder instances, take a look at "Hpgsql.Encoding".
--
-- = What's in this module
--
-- This module re-exports the essentials: connecting, querying, and the core types.
-- For more functionality, see:
--
-- * "Hpgsql.Query" for the @sql@ and @sqlPrep@ quasiquoters.
-- * "Hpgsql.Transaction" for transaction management.
-- * "Hpgsql.Pipeline" for pipelined queries.
-- * "Hpgsql.Copy" for @COPY@ protocol support.
-- * "Hpgsql.Pool" for connection pool utilities.
module Hpgsql
  ( -- * Connection
    connect,
    connectOpts,
    defaultConnectOpts,
    withConnection,
    withConnectionOpts,
    closeGracefully,
    closeForcefully,

    -- * Query
    query,
    queryWith,
    queryMWith,
    queryS,
    querySWith,
    querySMWith,
    query1,
    query1With,
    queryMay,
    queryMayWith,

    -- * Execute
    execute,
    execute_,
    executeMany,
    executeMany_,

    -- * Type info
    refreshTypeInfoCache,
    resetTypeInfoCache,
    getParameterStatus,
    getBackendPid,

    -- * Type re-exports
    HPgConnection, -- Do not export constructor
    Query, -- Do not export constructor
    ConnString (..),
    ConnectOpts (..),
    PostgresError (..),
    IrrecoverableHpgsqlError (..),
    ErrorDetail (..),
    RowDecoder (..),
    TransactionStatus (..),
    Only (..),
    FromPgRow (..),
    FromPgField (..),
  )
where

import Hpgsql.Encoding (FromPgField (..), FromPgRow (..), Only (..), RowDecoder (..))
import Hpgsql.Internal
import Hpgsql.InternalTypes (ConnString (..), ConnectOpts (..), ErrorDetail (..), HPgConnection, IrrecoverableHpgsqlError (..), PostgresError (..), TransactionStatus (..))
import Hpgsql.Query (Query)
