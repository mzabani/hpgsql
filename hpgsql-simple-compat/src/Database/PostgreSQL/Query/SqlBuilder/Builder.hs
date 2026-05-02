module Database.PostgreSQL.Query.SqlBuilder.Builder
  ( SqlBuilder,

    -- * Running
    runSqlBuilder,
  )
where

import Database.PostgreSQL.Query.SqlBuilder.Types (LogMasker)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.HpgsqlUtils (PgSimpleRow, toPgSimpleQuery)
import qualified Database.PostgreSQL.Simple.Types as PgSimple
import qualified Hpgsql.InternalTypes as HQ

-- | This is just an alias for hpgsql's `Query` type in hpgsql-simple-compat.
type SqlBuilder = HQ.Query

runSqlBuilder :: Connection -> LogMasker -> SqlBuilder -> IO (PgSimple.Query, PgSimpleRow)
runSqlBuilder _ _ q = pure $ toPgSimpleQuery q
