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
-- > import Hpgsql.Connection (withConnection, ConnectionString(..))
-- > import Hpgsql.Query (sql)
-- >
-- > main :: IO ()
-- > main = do
-- >   let connstr = ConnectionString
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
-- To embed a 'Query' fragment (e.g. an identifier or a sub-query), use @^{}@:
--
-- > let tableName = escapeIdentifier "users"
-- > rows <- query conn [sql|SELECT * FROM ^{tableName}|]
--
-- You can also use 'Hpgsql.Query.mkQuery' with dollar-numbered query arguments:
--
-- > query conn (mkQuery "SELECT * FROM users WHERE age BETWEEN $1 AND $2" (18 :: Int, 60 :: Int))
--
-- And finally, you can use the @[sqlPrep|...|]@ quasiquoter to build prepared statements.
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
-- = Handling errors
--
-- - Hpgsql is interruption-safe (with one exception for COPY inside transactions; see "Hpgsql.Copy"), so a query can be interrupted by asynchronous exceptions and you should still be able to run new queries on the same connection without any other side-effects. Naturally, it is up to you to determine which queries ran or not to completion, since they might have side-effects.
-- - Hpgsql will throw either `PostgresError` or `IrrecoverableHpgsqlError`, and:
--    - If you receive a `IrrecoverableHpgsqlError`, Hpgsql makes no promises about which statements ran to completion and what connection state is, and you should `closeForcefully` the connection without running any other queries. These errors should only be thrown for "obvious" developer mistakes from which usually there would be no way to proceed, anyway.
--    - If you receive a `PostgresError` exception, postgres and Hpgsql's states are synced and you can issue new queries afterwards.
--
-- = What's in this module
--
-- This module re-exports some of the essentials: querying and the core types.
-- For more functionality, see:
--
-- * "Hpgsql.Connection" for connecting and connection state resetting.
-- * "Hpgsql.Copy" for @COPY@ protocol support.
-- * "Hpgsql.Pipeline" for pipelined queries.
-- * "Hpgsql.Query" for the @sql@ and @sqlPrep@ quasiquoters.
-- * "Hpgsql.Transaction" for transaction management.
-- * "Hpgsql.Types" for extra types that might be useful.
module Hpgsql
  ( -- * Query
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
    ConnectionString (..),
    ConnectOpts (..),
    PostgresError (..),
    IrrecoverableHpgsqlError (..),
    ErrorDetail (..),
    RowDecoder (..),
    TransactionStatus (..),
    FromPgRow (..),
    FromPgField (..),
  )
where

import Hpgsql.Encoding (FromPgField (..), FromPgRow (..), RowDecoder (..))
import Hpgsql.Internal
import Hpgsql.InternalTypes (ConnectOpts (..), ConnectionString (..), ErrorDetail (..), HPgConnection, IrrecoverableHpgsqlError (..), PostgresError (..), TransactionStatus (..))
import Hpgsql.Query (Query)
