{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.PostgreSQL.Query.Types
  ( -- * Auxiliary types
    FN (..),
    fnToQuery,

    -- * Query execution
    HasPostgres (..),
    MonadPostgres,
    PgMonadT (..),
    runPgMonadT,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans (MonadTrans)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.List as L
import Database.PostgreSQL.Query.SqlBuilder (SqlBuilder (..), ToSqlBuilder (..), builderResultPure)
import Database.PostgreSQL.Simple.Internal (Connection)
import Hpgsql.InternalTypes (Query)
import Hpgsql.Query (escapeIdentifier)

{- | Dot-separated field name. Each element in nested list will be
properly quoted and separated by dot.

>>> "user.name" :: FN
FN ["user","name"]

>>> ("user" <> "name") :: FN
FN ["user","name"]

-}
newtype FN = FN [Text]
  deriving (Ord, Eq, Show, Semigroup, Monoid)

instance IsString FN where
  fromString s =
    FN $
      map T.pack $
        filter (/= ".") $
          L.groupBy f s
    where
      f a b = not $ a == '.' || b == '.'

instance ToSqlBuilder FN where
  toSqlBuilder fn = SqlBuilder $ \_ _ -> pure $ builderResultPure (fnToQuery fn)

-- | Convert a 'FN' to a 'Query' by quoting each part and joining with dots.
fnToQuery :: FN -> Query
fnToQuery (FN parts) =
  case parts of
    [] -> ""
    _ ->
      foldr1 (<>) $
        L.intersperse "." $
          map (escapeIdentifier . T.encodeUtf8) parts

-- | Instances of this typeclass can acquire a connection and pass it to
-- a computation.
class (MonadIO m) => HasPostgres m where
  withPGConnection :: (Connection -> m a) -> m a

type MonadPostgres m = HasPostgres m

-- | Reader of connection. Has instance of 'HasPostgres'. If you have a
-- connection you can run queries in this monad using 'runPgMonadT'.
newtype PgMonadT m a = PgMonadT
  { unPgMonadT :: ReaderT Connection m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadFail)

instance (MonadIO m) => HasPostgres (PgMonadT m) where
  withPGConnection action = do
    conn <- PgMonadT ask
    action conn
  {-# INLINEABLE withPGConnection #-}

runPgMonadT :: Connection -> PgMonadT m a -> m a
runPgMonadT conn (PgMonadT action) = runReaderT action conn
