module Database.PostgreSQL.Query.SqlBuilder.Builder
  ( SqlBuilder (..),

    -- * Running
    runSqlBuilder,
  )
where

import Data.String (IsString (..))
import Database.PostgreSQL.Query.SqlBuilder.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.HpgsqlUtils (PgSimpleRow, toPgSimpleQuery)

-- | In hpgsql-simple-compat the 'Connection' and 'LogMasker' parameters are
-- unused (logging is a no-op), but the type is preserved for API compatibility
-- with postgresql-query.
newtype SqlBuilder = SqlBuilder
  { sqlBuild :: Connection -> LogMasker -> IO SqlBuilderResult
  }

instance Semigroup SqlBuilder where
  (SqlBuilder a) <> (SqlBuilder b) =
    SqlBuilder $ \c masker -> (<>) <$> a c masker <*> b c masker

instance Monoid SqlBuilder where
  mempty = SqlBuilder $ \_ _ -> pure mempty

instance IsString SqlBuilder where
  fromString s = SqlBuilder $ \_ _ -> pure $ builderResultPure (fromString s)

-- | In hpgsql-simple-compat, this is very different
-- from in postgresql-simple because it returns both a query and the arguments
-- to run it. This happens because the core of the implementation - Hpgsql -
-- does not escape query arguments into the query string.
runSqlBuilder :: Connection -> LogMasker -> SqlBuilder -> IO (Query, PgSimpleRow)
runSqlBuilder conn masker (SqlBuilder bld) = toPgSimpleQuery . sbQueryString <$> bld conn masker
