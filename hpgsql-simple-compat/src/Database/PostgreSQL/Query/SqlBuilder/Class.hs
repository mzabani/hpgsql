module Database.PostgreSQL.Query.SqlBuilder.Class
  ( ToSqlBuilder (..),
  )
where

import Database.PostgreSQL.Query.SqlBuilder.Builder
import Database.PostgreSQL.Query.SqlBuilder.Types (builderResultPure)
import qualified Hpgsql.InternalTypes as HQ

-- | Typeclass for types convertible to 'SqlBuilder'.
class ToSqlBuilder a where
  toSqlBuilder :: a -> SqlBuilder

instance ToSqlBuilder SqlBuilder where
  toSqlBuilder = id

instance ToSqlBuilder HQ.Query where
  toSqlBuilder hq = SqlBuilder $ \_ _ -> pure $ builderResultPure hq
